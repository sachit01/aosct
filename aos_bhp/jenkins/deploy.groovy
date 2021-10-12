/************************************************************
*            DO NOT CHANGE THE NAME OF THIS JOB             *
*If the name changes it must be changed in the              *
*Build_onboard_Environments job currently on line 300       *
*************************************************************/

def getMasterJob()
{
    def testPrefix = ""
    def nameParts = env.JOB_BASE_NAME.split('_')
    if (nameParts[0].startsWith('Test'))
    {
        testPrefix = nameParts[0] + "_"
    }

    return testPrefix + "Build_Onboard_Environments"
}

//Parse version text file
def getVersionInfo(f)
{
    txt = readFile file: f
    def helper = { tag ->
        def re = (txt =~ /(?m)^\s*${tag}\s*:\s*(\w*(\s*\w+)+)\s*$/)
        if (re.size() == 1&& re[0].size() > 1)
        {
            return re[0][1]
        }
        else
        {
            currentBuild.result = 'ABORTED'
            error("Can't find ${tag} in versions.txt")
        }
    }

    return [ 'atc'     : helper('atc'),
             'atp_bhp' : helper('atp_bhp'),
             'atp_core': helper('atp_core'),
             'envs'    : helper('envs').split(' ')]
}

//regex to check the deploy IP target
def ip()
{
    return (("${params['deploy-ip']}" =~/^\.([0-9]{1,3})/)[0][1])
}

pipeline
{
    agent {label "deploy_agents"}

    options
    {
        disableConcurrentBuilds()

        buildDiscarder(logRotator(daysToKeepStr: '5'))

        // If not set, jenkins will try to checkout the
        // master repo by itself. Which we don't want, as
        // we manually clone in a later step.
        skipDefaultCheckout true
    }

    parameters
    {
        string(
            name: 'Build',
            description: "Which build of Build_Onboard_Environments shall be deployed?" )
        string(
            name: 'deploy-env',
            description: "Which environment to deploy? Can be left empty if target build only contains one " )
        choice(
            name: 'deploy-ip',
            choices:".36 (VSIM, Gurgaon)\n.44 (HIL, Gbg, bottom rack)\n.47 (HIL, Gbg, top rack)\n.52 (VSIM, Gbg)",
            description: "The rig to deploy to (the last number of the IP address)")
        // TODO: Fix this:
        // booleanParam(
        //     name: 'deploy-and-start',
        //     description: 'If set, the applications will be started after deploying')
    }

    //This is where the job was started from unless it was started by self
    environment
    {
        MasterJob = getMasterJob()
    }

    stages
    {
        stage('Get artifacts')
        {
            steps
            {
                script
                {
                    if (!params.Build)
                    {
                        currentBuild.result = 'ABORTED'
                        error('A valid build to deploy must be specified')
                    }
                }

                //Get the artifacts from a specific buildnumber ${params.Build}
                //This is send down from the MasterJob if that is what initiated the build
                dir('artifacts')
                {
                    copyArtifacts projectName: MasterJob,
                        flatten:true,
                        filter: "packages_*.zip",
                        selector: specific(buildNumber: "${params.Build}")

                    copyArtifacts projectName: MasterJob,
                        flatten:true,
                        filter: "extras.zip",
                        selector: specific(buildNumber: "${params.Build}")
                }

                dir('build')
                {
                    script
                    {
                        //Unzip the extras and grab some information
                        sh "unzip \"../artifacts/extras.zip\""
                        def envs = getVersionInfo('versions.txt')['envs']
                        def e = ""

                        //Make sure that the deploy-env was build in the buildnumber from the Masterjob
                        if (params['deploy-env'])
                        {
                            if (!(params['deploy-env'] in envs))
                            {
                                currentBuild.result = 'ABORTED'
                                error("params['deploy-env'] was not built by ${MasterJob}#${params.Build}")
                            }
                            e = params['deploy-env']

                            echo "Using ${e} as specified"
                        }
                        else
                        {
                            //If there was only one build in the masterjob
                            if (envs.size() == 1)
                            {
                                e = envs[0]
                                echo "No env specified, defaulting to ${e}"
                            }
                            else
                            {
                                currentBuild.result = 'ABORTED'
                                error('No env specified and target build contains several environments, can\'t decide which to use')
                            }
                        }

                        //HTML! Beautiful HTML
                        currentBuild.description = """
                            Deploying ${e} from
                            <a href=${JENKINS_URL}/job/${MasterJob}/${params.Build}/">${MasterJob} #${params.Build}</a>
                        """

                        sh "unzip \"../artifacts/packages_${e}.zip\""
                        writeFile file: "env.txt", text: "${e}"
                    }

                }
            }
        }

        //This job can only be run on machines / docker containers that are specified to VPN to the racks
        stage('Deploy')
        {
            steps
            {
                dir('build')
                {
                    script
                    {
                        //Read env.txt and determine the last octet of the IP in the rack to deploy too
                        def env = readFile file: 'env.txt'
                        def clientip = ip()
                        //initiate the VPN connection and sleep for 10 seconds to let it establish
                        sh "pon client${clientip}"
                        sleep time: 10
                        //deploy the build
                        sh "make deploy-${env.toLowerCase()}"
                    }
                }
            }
        }

        stage('Startup')
        {
            steps
            {
                dir('build')
                {
                    script
                    {
                        //if the parameter deploy-and-start was set, run "make start" to start up
                        if (params['deploy-and-start'])
                        {
                            //sleep time: 1
                            echo "deploy-and-start was set"
                            echo "TODO: This is currently not working as it should..."
                            /*echo "deploying a"
                            sh "sshpass -p atpcu ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null atpcu@192.168.2.10 /opt/bin/startup_system.sh&"
                            echo "deploying b"
                            sh "sshpass -p atpcu ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null atpcu@192.168.2.11 /opt/bin/startup_system.sh&"
                            echo "deploying c"
                            sh "sshpass -p atpcu ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null atpcu@192.168.2.12 /opt/bin/startup_system.sh&"
                            //sh "./start_all.sh"                            */
                        }
                    }
                }
            }
        }
    }

    post
    {
        cleanup
        {
            //Clean up, make sure we disconnect the VPN connection!!
            sh returnStatus: true, script: "poff client${ip()}"
            sh 'rm -rf ./artifacts'
            sh 'rm -rf ./build'
        }
    }
}
