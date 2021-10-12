/************************************************************
*            DO NOT CHANGE THE NAME OF THIS JOB             *
*************************************************************/
def getEnvVar()
{
    def nameParts = env.JOB_BASE_NAME.split('_')
    def i = 0

    if (nameParts[i].startsWith('Test'))
    {
        i += 1
    }

    if (nameParts[i] == 'Build'
    &&  nameParts[i+1] == 'Dispatcher'
    &&  ['VSIM','EMD','HIL'].contains(nameParts[i+2]))
    {
        return nameParts[i+2]
    }
    else
    {
         currentBuild.result = 'ABORTED'
         error('${env.JOB_BASE_NAME} is a invalid jobname, should be named as [Test_]Build_Dispatcher_ENV')
    }
}

pipeline
{
    agent { label 'build' }

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
        def envi = ENV.toLowerCase()
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
                        def repos = ['atc', 'atp_core', 'dispatcher']
                        sh "rm -rf aos_bhp"
                        sh "mkdir -p aos_bhp"

                        for (repo in repos)
                        {
                            copyArtifacts projectName: 'Checkout_AOS', filter: "${repo}.tar.gz", selector: specific(buildNumber: params.checkoutJobNo)
                            sh "tar --extract --file ${repo}.tar.gz && rm ${repo}.tar.gz"
                            sh "mv ${repo} aos_bhp"
                        }
                    }
                }
            }
        }

        stage('Build')
        {
            steps
            {
                dir('build/aos_bhp/dispatcher/impl')
                {
                    sh "make -j4 TARGET=${env.TARGET} ENV=${env.ENV} PROJS_ROOT=${workspace}/build |& tee ../../../build_log.txt"
                }
            }
        }

        stage('Check Build')
        {
            steps
            {
                script
                {
                    dir('build')
                    {
                        def re = "(^.*:([0-9]+:)?[0-9]+:) warning:(.*)"
                        sh  "cat build_log.txt | grep -E \"${re}\" > warn_log.txt || true"
                        if (fileExists([file: 'warn_log.txt']))
                        {
                            def warnTxt = readFile file:'./warn_log.txt'
                            def warnings = warnTxt.split('\n')
                            if (warnings.size() > 0 && warnTxt.trim() != "")
                            {
                                dir("warnings/${env.JOB_NAME}/")
                                {
                                    writeFile file: 'warnings.txt', text: "${env.JOB_NAME}:${env.BUILD_NUMBER}\n"+warnTxt
                                    def desc = "<p><b>Warnings</b></p>"
                                    for (w in warnings)
                                    {
                                        def wr = (w =~ re)
                                        //Make path relative workspace directory
                                        path = wr[0][1] - ~"^(/[^/]*)*/${env.JOB_NAME}/"
                                        warn = wr[0][3]
                                        desc += """
                                            <div style="margin-bottom:1em;">
                                                <span style="color:#818181;"> ${path} </span>
                                                <br />
                                                ${warn}
                                                <br />
                                            </div>
                                        """
                                    }
                                    currentBuild.description=desc
                                    currentBuild.result = 'FAILURE'
                                }
                            }
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
                sh "zip -R target_dispatcher_${env.TARGET}_${env.ENV} aos_bhp/dispatcher/target/${env.envi}/dispatcher*"
                archiveArtifacts artifacts: "target_dispatcher_${env.TARGET}_${env.ENV}.zip", fingerprint: true
            }
        }

        cleanup
        {
            sh "rm -rf build"
        }
    }
}
