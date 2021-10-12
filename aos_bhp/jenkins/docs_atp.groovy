/************************************************************
*            DO NOT CHANGE THE NAME OF THIS JOB             *
*************************************************************/
//Parse the name of the job to get env and target/$
//The name should be Docs_ATP_(PLATFORM)_(ENVIRONMENT)
//Where PLATFORM is ARM or PPC and ENVIRONMENT is VSIM, EMD or HIL
def getEnvVar()
{
    def nameParts = env.JOB_BASE_NAME.split('_')
    def i = 0

    if (nameParts[i].startsWith('Test'))
    {
        i += 1
    }

    if (nameParts[i] == 'Docs'
    &&  nameParts[i+1] == 'ATP'
    &&  ['VSIM','EMD','HIL'].contains(nameParts[i+2]))
    {
        return nameParts[i+2]
    }
    else
    {
         currentBuild.result = 'ABORTED'
         error('${env.JOB_BASE_NAME} is a invalid jobname, should be named as [Test_]Docs_ATP_ENV')
    }
}

pipeline
{
    agent { label 'docs' }

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
        def ENV = getEnvVar()
        def TARGET = "PPC"
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
                        def repos = ['aos_bhp', 'atc', 'atp_bhp', 'atp_core']
                        sh "mkdir -p aos_bhp"

                        for (repo in repos)
                        {
                            copyArtifacts projectName: 'Checkout_AOS', filter: "${repo}.tar.gz", selector: specific(buildNumber: params.checkoutJobNo)
                            sh "tar --extract --file ${repo}.tar.gz && rm ${repo}.tar.gz"
                            if (repo != 'aos_bhp')
                            {
                                sh "mv ${repo} aos_bhp"
                            }
                        }
                    }
                }
            }
        }

        stage('Docs')
        {
            steps
            {
                dir('build/aos_bhp/atp_bhp/impl')
                {
                    sh "make -j4 docs TARGET=${env.TARGET} ENV=${env.ENV} PROJS_ROOT=${workspace}/build"

                    // Fix for fixing the links to individual pages:
                    sh "make TARGET=${env.TARGET} ENV=${env.ENV} PROJS_ROOT=${workspace}/build ${workspace}/build/aos_bhp/atc/spec/output/logs/atc_all/atc_all/DUMMY"
                    sh "make TARGET=${env.TARGET} ENV=${env.ENV} PROJS_ROOT=${workspace}/build ${workspace}/build/aos_bhp/atp_core/spec/output/logs/atp_core_all/atp_core_all/DUMMY"
                    sh "make TARGET=${env.TARGET} ENV=${env.ENV} PROJS_ROOT=${workspace}/build ${workspace}/build/aos_bhp/atp_bhp/spec/output/logs/atp_bhp_all/atp_bhp_all/DUMMY"
                }
            }
        }
    }

    // On success we archive the artifacts so that other jobs can access them
    post
    {
        success
        {
            dir('build/aos_bhp/atc/spec/output/atc_all')
            {
                sh "zip -r atc_docs.zip * > /dev/null"
                archiveArtifacts artifacts: "atc_docs.zip", fingerprint: true
            }

            dir('build/aos_bhp/atp_core/spec/output/atp_core_all')
            {
                sh "zip -r atp_core_docs.zip * > /dev/null"
                archiveArtifacts artifacts: "atp_core_docs.zip", fingerprint: true
            }

            dir('build/aos_bhp/atp_bhp/spec/output/atp_bhp_all')
            {
                sh "zip -r atp_bhp_docs.zip * > /dev/null"
                archiveArtifacts artifacts: "atp_bhp_docs.zip", fingerprint: true
            }
        }

        cleanup
        {
            sh 'rm -rf build'
        }
    }
}
