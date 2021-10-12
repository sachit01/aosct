/************************************************************
*            DO NOT CHANGE THE NAME OF THIS JOB             *
*If the name changes it must be changed in the Deploy Job   *
*around line 40 as well to match!                           *
*************************************************************/

//Get the environments from the parameter section
def getEnvironments()
{
    if (params.GERRIT_CHANGE_NUMBER || params.Environment == 'All')
    {
        return [ 'HIL','EMD','VSIM','SIL' ]
    }
    else
    {
        return [ params.Environment ]
    }
}

def getTargetEnvironments()
{
    def environments = getEnvironments()
    def result = []

    for (environment in environments)
    {
        if (environment != 'SIL')
        {
            result += environment
        }
    }

    return result
}

def getVersion()
{
    if (params.GERRIT_CHANGE_NUMBER)
    {
        return "automatic_build_bot"
    }
    else
    {
        return params.Version
    }
}

def getTestPrefix()
{
    def testPrefix = ""
    def nameParts = env.JOB_BASE_NAME.split('_')
    if (nameParts[0].startsWith('Test'))
    {
        testPrefix = nameParts[0] + "_"
    }

    return testPrefix
}

// Returns a list of job names for the ATP application
// The names have the form [Test_](Build|Lint)_ATP_TARGET_ENVIRONMENT
def getAtpJobs()
{
    def environments = getTargetEnvironments()
    def targets = [ "PPC", "ARM" ]
    def jobs = []

    for (cenv in environments)
    {
        if (cenv == "VSIM")
        {
            jobs += getTestPrefix() + "Docs_ATP_${cenv}"
        }

        for (ctgt in targets)
        {
            jobs += getTestPrefix() + "Build_ATP_${ctgt}_${cenv}"

            if (cenv == "VSIM")
            {
                jobs += getTestPrefix() + "Lint_ATP_${ctgt}_${cenv}"
            }
        }
    }

    return jobs
}

// Returns a list of job names for the Dispatcher application
// The names have the form [Test_](Build|Lint)_Dispatcher_ENVIRONMENT
def getDispatcherJobs()
{
    def environments = getTargetEnvironments()
    def jobs = []

    for (cenv in environments)
    {
        jobs += getTestPrefix() + "Build_Dispatcher_${cenv}"

        if (cenv == "VSIM")
        {
            jobs += getTestPrefix() + "Docs_Dispatcher_${cenv}"
            jobs += getTestPrefix() + "Lint_Dispatcher_${cenv}"
        }
    }

    return jobs
}

def getWindowsJobs()
{
    def jobs = []

    if (getEnvironments().contains('SIL'))
    {
        jobs += getTestPrefix() + "Build_Windows"
    }

    return jobs
}

//Create a parallellmap to run pipeline in parallell
def parallelStagesMap(checkoutJobNo, prereqJobNo)
{
    def dependentJobs = getWindowsJobs() + getAtpJobs() + getDispatcherJobs()

    return dependentJobs.collectEntries {
        ["${it}" : generateStage(it, checkoutJobNo, prereqJobNo)]
    }
}

def generateStage(jobName, checkoutJobNo, prereqJobNo)
{
    return {
        stage("stage: ${jobName}")
        {
            script
            {
                def res = build job: jobName, parameters:
                [
                    string( name: 'checkoutJobNo', value: "${checkoutJobNo}"),
                    string( name: 'prereqJobNo', value: "${prereqJobNo}")
                ]

                copyArtifacts projectName: jobName,
                    flatten: true,
                    filter: "*",
                    selector: specific(buildNumber: res.getNumber().toString())
            }
        }
    }
}

//function to check out single repo from git
def checkoutRepo(repoName,branchName)
{
    dir("${repoName}")
    {
        def name = branchName.replaceAll("^change: *([0-9]{3}([0-9]{2}))/([0-9]+)\$","refs/remotes/origin/changes/\$2/\$1/\$3")
        echo "git: ${repoName}/${name}"
        def ret = checkout([ scm: [ $class: 'GitSCM',doGenerateSubmoduleConfigurations: false, name: "${repoName}", branches: [[name: name]],
                                    extensions: [[$class: 'SubmoduleOption', parentCredentials: true]],
                                    userRemoteConfigs: [[ refspec: "+refs/heads/*:refs/remotes/origin/* +refs/changes/*:refs/remotes/origin/changes/*",
                                                          url: "ssh://10.160.155.37:29418/${repoName}", credentialsId: 'ed5f023f-54c8-41f2-b794-cc0dc5d96a51']] ] ])
        return ret.GIT_COMMIT;
    }
}

//function to create a collection of repos to check out from git
def checkoutRepos(repos)
{
    return repos.collectEntries {
        [(it.toString()): checkoutRepo(it,"${params[it]}") ]
    }
}

//Function to checkout aos_bhp and figure out the commit hashes for the submodules
def checkoutMasterRepo(repos)
{
    def branch = ""
    if (params.GERRIT_CHANGE_NUMBER)
    {
        //Our build has been triggered by the gerrit plugin
        //Thus let's change the branch name to point at the change
        //that caused the build to run
        def cnumber=sprintf("%05d",Integer.parseInt(params.GERRIT_CHANGE_NUMBER))
        branch = "change: ${cnumber}/${params.GERRIT_PATCHSET_NUMBER}"
    }
    else
    {
        branch = params.aos_bhp
    }

    def master = checkoutRepo('aos_bhp', branch);
    def hashes = ['aos_bhp': master]

    dir("aos_bhp/")
    {
        for (repo in repos)
        {
            hashes["${repo}"] = sh(returnStdout: true, script: " git rev-parse --verify @:./${repo}").trim()
        }
    }

    return hashes
}

pipeline
{
    agent any

    options
    {
        disableConcurrentBuilds()

        buildDiscarder(logRotator(daysToKeepStr: '15'))

        // If not set, jenkins will try to checkout the
        // master repo by itself. Which we don't want, as
        // we manually clone in a later step.
        skipDefaultCheckout true
    }

    //Parameters, should be equal to the ones set in build with parameters in the main section of Jenkins
    parameters
    {
        choice(
            name: 'Environment',
            choices:"All\nEMD\nHIL\nVSIM\nSIL",
            description: "Which environment?" )
        string(
            name: 'Version',
            description: "Version X.Y.Z",
            defaultValue: 'manual_build')
        string(
            name: 'aos_bhp',
            description: 'Commit id, tag, branch or "change: change_number/patch_number" (If this is specified the atc, atp_bhp, atp_core and dispatcher parameters are ignored)',
            defaultValue: 'master')
        string(
            name: 'atc',
            description: 'Commit id, tag, branch or "change: change_number/patch_number"',
            defaultValue: 'master')
        string(
            name: 'atp_bhp',
            description: 'Commit id, tag, branch or "change: change_number/patch_number"',
            defaultValue: 'master')
        string(
            name: 'atp_core',
            description: 'Commit id, tag, branch or "change: change_number/patch_number"',
            defaultValue: 'master')
        string(
            name: 'dispatcher',
            description: 'Commit id, tag, branch or "change: change_number/patch_number"',
            defaultValue: 'master')
        string(
            name: 'dmi_bhp',
            description: 'Commit id, tag, branch or "change: change_number/patch_number"',
            defaultValue: 'master')
        string(
            name: 'tools_bhp',
            description: 'Commit id, tag, branch or "change: change_number/patch_number"',
            defaultValue: 'master')
        booleanParam(
            name: 'deploy',
            description: 'If set, the build will be deployed on "deploy-ip" (see below)')
        choice(
            name: 'deploy-ip',
            choices:".36 (VSIM, Gurgaon)\n.44 (HIL, Gbg, bottom rack)\n.47 (HIL, Gbg, top rack)\n.52 (VSIM, Gbg)",
            description: "The rig to deploy to (the last number of the IP address)")
        // TODO: Fix this:
        // booleanParam(
        //     name: 'deploy-and-start',
        //     description: 'If set, the applications will be started after deploying')
    }

    //Start of the pipeline
    stages
    {
        stage('Trigger builds')
        {
            steps
            {
                script
                {
                    //Check that parameters are set correctly
                    if (params.GERRIT_CHANGE_NUMBER)
                    {
                        echo "change nr: ${GERRIT_CHANGE_NUMBER}"
                        echo "refspec: ${GERRIT_REFSPEC}"
                    }

                    if (!((params.atc && params.atp_core && params.atp_bhp && params.dispatcher && params.dmi_bhp && params.tools_bhp) || params.aos_bhp))
                    {
                        currentBuild.result = 'ABORTED'
                        error('Commits to use not specified, aborting ')
                    }

                    def use_aos_bhp = false
                    if ((params.aos_bhp && params.aos_bhp.size() > 0) || params.GERRIT_CHANGE_NUMBER)
                    {
                        use_aos_bhp = true
                    }

                    def hashes = []
                    def d_commits = ""
                    def checkoutJobNo = ""
                    def prereqJobNo = ""

                    //This section should fetch dependencies from Synergy, currently not working properly
                    //Currently just fetches a zip-file
                    dir('tmp')
                    {
                        def res = build job: 'fetch_dependencies'
                        prereqJobNo = res.getNumber().toString()

                        copyArtifacts projectName: 'fetch_dependencies',
                                      filter: "dependencies.zip",
                                      selector: specific(buildNumber: prereqJobNo)
                    }

                    //This is where we start checking out repos from git
                    dir('repos')
                    {
                        def repos = ['atc', 'atp_bhp', 'atp_core', 'dispatcher', 'dmi_bhp', 'tools_bhp']

                        if (use_aos_bhp)
                        {
                            hashes = checkoutMasterRepo(repos);
                            dir('../build/aos_bhp')
                            {
                                for (repo in repos)
                                {
                                    sh "cp -r ../../repos/aos_bhp/${repo} ."
                                }
                            }
                        }
                        else
                        {
                            hashes = checkoutRepos(repos);
                            dir('../build/aos_bhp')
                            {
                                for (repo in repos)
                                {
                                    sh "cp -r ../../repos/${repo} ."
                                }
                            }
                        }

                        // Temporary fix for building without LSSD:
                        dir('../build/aos_bhp')
                        {
                            sh "cp -p -r atp_bhp/target atp_bhp/target_with_lssd"
                        }

                        //After we check out the repos we build some HTML for Jenkins to show the builder
                        //by giving links to the git commits that were used
                        def d_links =""
                        def d_names =""
                        def d_param_names =""
                        def d_params = ""
                        for (w in repos)
                        {
                            d_links+= """
                               <a href="https://10.160.155.37/review/#/q/${hashes[w]}">${hashes[w]}</a><br />
                            """
                            d_names += """
                               ${w}:<br />
                            """

                            if (!use_aos_bhp)
                            {
                                d_params += """
                                    ${params[w]}<br />
                                """
                            }

                            d_commits += """
                                ${w}:${hashes[w]}
                            """
                        }

                        if (use_aos_bhp)
                        {
                            d_param_names = "aos_bhp:"
                            if (params.GERRIT_CHANGE_URL)
                            {
                                d_params = "<a href=\"${params.GERRIT_CHANGE_URL}\">Change ${params.GERRIT_CHANGE_NUMBER}</a>"
                                if (params.GERRIT_CHANGE_SUBJECT)
                                {
                                    d_params += "<br />${params.GERRIT_CHANGE_SUBJECT}"
                                    d_param_names += "<br />&nbsp;"
                                }
                            }
                            else
                            {
                                d_params = "${params.aos_bhp}"
                            }
                        }
                        else
                        {
                            d_param_names = d_names
                        }

                        currentBuild.description = """
                            <b>Environment:</b> ${getEnvironments().join(' ')}<br />
                            <b>Version:</b> ${getVersion()}<br />

                            <h3>Selected names</h3>
                            <div style="display: inline-block; font-weight: bold;">
                                ${d_param_names}
                            </div>
                            <div style="display: inline-block">
                                ${d_params}
                            </div>
                            <h3>Commits used</h3>
                            <div style="display: inline-block; font-weight: bold;">
                                ${d_names}
                            </div>
                            <div style="display: inline-block">
                                ${d_links}
                            </div>
                            <h3>Dependencies</h3>
                            <a href="${JENKINS_URL}job/fetch_dependencies/${prereqJobNo}/">fetch_dependencies #${prereqJobNo}</a>
                        """
                    }

                    dir('build/extras')
                    {
                        //Write version information to file
                        //This can be used by other jobs to figure out
                        //what commits was used to build this
                        writeFile file: 'versions.txt',
                            text: """
                                envs: ${getEnvironments().join(' ')}
                                ${d_commits}
                            """
                    }

                    // We use a centralized checkout job to avoid having lots of repo clones
                    dir('tmp')
                    {
                        def aos_rev = 'master'
                        if (use_aos_bhp)
                        {
                           aos_rev = hashes['aos_bhp']
                        }

                        def res = build job: 'Checkout_AOS', parameters:
                        [
                            string( name: 'git_aos_bhp',    value: "${aos_rev}" ),
                            string( name: 'git_atc',        value: "${hashes['atc']}"        ),
                            string( name: 'git_atp_core',   value: "${hashes['atp_core']}"   ),
                            string( name: 'git_atp_bhp',    value: "${hashes['atp_bhp']}"    ),
                            string( name: 'git_dispatcher', value: "${hashes['dispatcher']}" ),
                            string( name: 'git_dmi_bhp',    value: "${hashes['dmi_bhp']}"    ),
                            string( name: 'git_tools_bhp',  value: "${hashes['tools_bhp']}"  )
                        ]

                        checkoutJobNo = res.getNumber().toString()
                    }

                    dir('tmp')
                    {
                        //This line triggers the buildjobs that correspond to the environment and target that was defined
                        parallel parallelStagesMap(checkoutJobNo, prereqJobNo)
                    }
                }
            }
        }

        stage('Collect results')
        {
            steps
            {
                script
                {
                    //The builds should be done, unzip the result
                    dir('build')
                    {
                        if (getEnvironments().contains('SIL'))
                        {
                            sh 'mv ../tmp/aos_bhp_win32.zip .'
                            archiveArtifacts artifacts: "aos_bhp_win32.zip", fingerprint: true
                        }
                        if (getEnvironments().contains('VSIM'))
                        {
                            sh 'mv ../tmp/*_docs.zip .'
                            archiveArtifacts artifacts: "*_docs.zip", fingerprint: true
                        }

                        sh 'ls ../tmp/*.zip | xargs -L 1 unzip'
                        def jobs = getAtpJobs()
                        def warnLinks = ""
                        def warnS =""
                        //Add some nice HTML to view in Jenkins for debugging purpouses
                        for (j in jobs)
                        {
                            def f = "warnings/${j}/warnings.txt"
                            def fEx = fileExists file: f
                            if (fEx)
                            {
                                def ws = readFile([file: f]).split('\n')
                                def bi = ws[0].split(':')
                                def wlen = ws.length-1;
                                warnLinks += """
                                    <a href="${JENKINS_URL}job/${bi[0]}/${bi[1]}/">${bi[0]}</a>
                                    <br />
                                """
                                warnS+="""
                                    <span style="color:orange;" >warnings: ${wlen}</span>
                                    <br />
                                """
                            }

                        }
                        if (warnLinks != "")
                        {
                            currentBuild.description += """
                            <h3> Warnings </h3>
                            <div style="display:inline-block;">
                                ${warnLinks}
                            </div>
                            <div style="display:inline-block;">
                                ${warnS}
                            </div>
                            """
                        }
                    }
                }
            }
        }

        //Builds are done, warnings have been printed. Time to run the Makefile
        stage('Packaging')
        {
            steps
            {
                script
                {
                    if (getTargetEnvironments().size() > 0)
                    {
                        //Run the makefile with Version set in the parameters for the environments that was specified
                        dir('build/aos_bhp/atp_bhp/target')
                        {
                            sh "make packages VER=${getVersion()} ENVLIST=\"${getTargetEnvironments().join(' ')}\" PROJS_ROOT=${workspace}/build"
                        }

                        // Temporary fix for building without LSSD:
                        dir('build/aos_bhp/atp_bhp')
                        {
                            sh "mv target target_without_lssd"
                            sh "mv target_with_lssd target"
                        }
                        dir('build/aos_bhp/atp_bhp/target')
                        {
                            sh "make packages VER=${getVersion()} ENVLIST=\"${getTargetEnvironments().join(' ')}\" PROJS_ROOT=${workspace}/build"
                        }
                        dir('build/aos_bhp/atp_bhp')
                        {
                            sh "mv target target_with_lssd"
                            sh "mv target_without_lssd target"
                        }
                    }
                }
            }
        }
    }

    //Building, Making and Deploying finished, time to clean up after ourselves
    post
    {
        success
        {
            dir('build/extras')
            {
                sh 'cp ../aos_bhp/atp_bhp/target/Makefile ../aos_bhp/atp_bhp/target/*.sh .'
            }

            dir('build')
            {
                zip zipFile: "extras.zip",
                    archive: true,
                    dir: "extras"
            }

            dir('build/data_files')
            {
                sh 'cp ../aos_bhp/atp_bhp/nvshft/data_files/* .'
            }

            dir('build')
            {
                zip zipFile: "data_files.zip",
                    archive: true,
                    dir: "data_files"
            }

            dir('build/aos_bhp/atp_bhp/target')
            {
                script
                {
                    for (e in getTargetEnvironments())
                    {
                        def packSuffix = e == "VSIM" ? "" : "_" + e.toLowerCase()
                        zip zipFile: "packages_${e}.zip",
                            archive: true,
                            glob: "install_packages${packSuffix}/*"
                    }
                }
            }

            // Temporary fix for building without LSSD:
            dir('build/aos_bhp/atp_bhp/target_with_lssd')
            {
                script
                {
                    for (e in getTargetEnvironments())
                    {
                        def packSuffix = e == "VSIM" ? "" : "_" + e.toLowerCase()
                        zip zipFile: "packages_${e}_with_lssd.zip",
                            archive: true,
                            glob: "install_packages${packSuffix}/*"
                    }
                }
            }

            dir('tmp')
            {
                script
                {
                    for (e in getTargetEnvironments())
                    {
                        if (e == "VSIM")
                        {
                            sh 'cat lint_atp_ARM_VSIM_warnings.txt >  lint_log.txt'
                            sh 'echo                               >> lint_log.txt'
                            sh 'cat lint_atp_PPC_VSIM_warnings.txt >> lint_log.txt'

                            archiveArtifacts artifacts: "lint_log.txt", fingerprint: true
                            archiveArtifacts artifacts: "lint_suppressions.xlsx", fingerprint: true
                        }
                    }
                }
            }

            //Generate HTML code to show in jenkins where (if) build was deployed
            script
            {
                if (params.deploy)
                {
                    if (getTargetEnvironments().size() != 1)
                    {
                        echo "Skipping deploy, more than one env built"
                        currentBuild.description += """
                            <h3>Deployment info</h3>
                            Skipped, all builds can't be deployed
                        """
                    }
                    else
                    {
                        def jobName = getTestPrefix() + "Deploy_Build"

                        def res = build job: jobName, parameters: [
                            string(name: 'deploy-ip',value: "${params['deploy-ip']}"),
                            string(name: 'Build', value: "${env.BUILD_NUMBER}"),
                            string(name: 'deploy-env', value: "${getTargetEnvironments()[0]}"),
                            booleanParam(name: 'deploy-and-start', value: params['deploy-and-start'])
                        ]

                        currentBuild.description += """
                            <h3>Deployment info</h3>
                            Deployed using
                            <a href="${JENKINS_URL}/job/deploy_build/${res.getNumber()}/">deploy_build #${res.getNumber()}</a>
                            at ${params['deploy-ip']}
                        """
                    }
                }
            }
        }

        //Always clean up after running, even on fails
        cleanup
        {
            sh 'rm -rf ./tmp'
            sh 'rm -rf ./build'
        }
    }
}
