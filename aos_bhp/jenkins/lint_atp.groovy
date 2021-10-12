/************************************************************
*            DO NOT CHANGE THE NAME OF THIS JOB             *
*************************************************************/
//Parse the name of the job to get env and target/$
//The name should be Lint_ATP_(PLATFORM)_(ENVIRONMENT)
//Where PLATFORM is ARM or PPC and ENVIRONMENT is VSIM, EMD or HIL
def getVars(var)
{
    def nameParts = env.JOB_BASE_NAME.split('_')
    def i = 0

    if (nameParts[i].startsWith('Test'))
    {
        i += 1
    }

    if (nameParts[i] == 'Lint'
    &&  nameParts[i+1] == 'ATP'
    &&  ['ARM','PPC'].contains(nameParts[i+2])
    &&  ['VSIM','EMD','HIL'].contains(nameParts[i+3]))
    {
        return ['env' : nameParts[i+3], 'tgt' : nameParts[i+2]][var]
    }
    else
    {
         currentBuild.result = 'ABORTED'
         error('${env.JOB_BASE_NAME} is a invalid jobname, should be named as [Test_]Lint_ATP_PLATFORM_ENV')
    }
}

def generateHtmlFromFile(filename)
{
    def html = ""

    if (fileExists([file: filename]))
    {
        def content = readFile file: filename
        def lines = content.split('\n')
        if (lines.size() > 0 && content.trim() != "")
        {
            for (line in lines)
            {
                if (line.trim() != "")
                {
                    html += "<p>"
                    html += line
                    html += "</p>"
                }
            }
        }
    }

    return html
}

pipeline
{
    agent { label 'lint' }

    options
    {
        disableConcurrentBuilds()

        buildDiscarder(logRotator(daysToKeepStr: '5'))

        // If not set, jenkins will try to checkout the
        // master repo by itself. Which we don't want, as
        // we manually clone in a later step.
        skipDefaultCheckout true
    }

    environment
    {
        def ENV = getVars('env')
        def TARGET = getVars('tgt')
        def OUT_NAME = "lint_atp_${TARGET}_${ENV}"
        def FIND_SUPPRESSIONS = 'tools_bhp/LintStatistics/findSuppressions.py'
        def LINT_SUPPRESSIONS = 'lint_suppressions.xlsx'
    }

    stages
    {
        stage('Setup')
        {
            steps
            {
                script
                {
                    dir('build')
                    {
                        def repos = ['aos_bhp', 'atc', 'atp_bhp', 'atp_core', 'tools_bhp']
                        sh "mkdir -p aos_bhp"

                        for (repo in repos)
                        {
                            copyArtifacts projectName: 'Checkout_AOS', filter: "${repo}.tar.gz", selector: specific(buildNumber: params.checkoutJobNo)

                            if (repo == 'tools_bhp')
                            {
                                sh "tar --extract --file ${repo}.tar.gz ${env.FIND_SUPPRESSIONS} && rm ${repo}.tar.gz"
                            }
                            else
                            {
                                sh "tar --extract --file ${repo}.tar.gz && rm ${repo}.tar.gz"
                            }

                            if (repo != 'aos_bhp')
                            {
                                sh "mv ${repo} aos_bhp"
                            }
                        }
                    }
                }
            }
        }

        // Grab the prerequisite files, currently they are just zipped on a remote machine. Subject to change (?)
        stage('Prerequisites')
        {
            steps
            {
                dir('build')
                {
                    copyArtifacts projectName: 'fetch_dependencies',
                        filter: "dependencies.zip",
                        selector: specific(buildNumber: params.prereqJobNo)
                    sh "unzip dependencies.zip"
                }
            }
        }

        stage('Lint')
        {
            steps
            {
                dir('build/aos_bhp/atp_bhp/impl')
                {
                    sh "make -j4 lint TARGET=${env.TARGET} ENV=${env.ENV} PROJS_ROOT=${workspace}/build |& tee ../../../${env.OUT_NAME}_warnings.txt"
                }

                dir('build/aos_bhp')
                {
                    script
                    {
                        if (env.TARGET == 'PPC')
                        {
                            sh "python ${env.FIND_SUPPRESSIONS} . ${env.LINT_SUPPRESSIONS}"
                        }
                    }
                }
            }
        }

        stage('Check Lint')
        {
            steps
            {
                script
                {
                    dir('build')
                    {
                        sh "python aos_bhp/scripts/checkLintWarnings.py --only-fatal ${env.OUT_NAME}_warnings.txt |& tee ${env.OUT_NAME}_errors.txt"

                        def desc = generateHtmlFromFile("${env.OUT_NAME}_errors.txt")
                        if (desc != "")
                        {
                            if (currentBuild.description == null)
                            {
                                currentBuild.description = ""
                            }
                            currentBuild.description += '<p><b>Lint errors</b></p>'
                            currentBuild.description += desc
                            error "Lint errors found, aborting"
                        }
                    }
                }
            }
        }
    }

    // On success we archive the artifacts so that other jobs can access them
    post
    {
        success
        {
            dir('build')
            {
                archiveArtifacts artifacts: "${env.OUT_NAME}_warnings.txt", fingerprint: true
            }

            dir('build/aos_bhp')
            {
                script
                {
                    if (env.TARGET == 'PPC')
                    {
                        archiveArtifacts artifacts: "${env.LINT_SUPPRESSIONS}", fingerprint: true
                    }
                }
            }
        }

        cleanup
        {
            sh 'rm -rf build'
        }
    }
}
