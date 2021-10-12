/************************************************************
*            DO NOT CHANGE THE NAME OF THIS JOB             *
*************************************************************/
//Define cpu_A = PPC, cpu_B = ARM
def toCPU(target)
{
    return "cpu_" + ((target == "PPC") ? "a" : "b")
}

//Parse the name of the job to get env and target/$
//The name should be Build_ATP_(PLATFORM)_(ENVIRONMENT)
//Where PLATFORM is ARM or PPC and ENVIRONMENT is VSIM, EMD or HIL
def getVars(var)
{
    def nameParts = env.JOB_BASE_NAME.split('_')
    def i = 0

    if (nameParts[i].startsWith('Test'))
    {
        i += 1
    }

    if (nameParts[i] == 'Build'
    &&  nameParts[i+1] == 'ATP'
    &&  ['ARM','PPC'].contains(nameParts[i+2])
    &&  ['VSIM','EMD','HIL'].contains(nameParts[i+3]))
    {
        return ['env' : nameParts[i+3], 'tgt' : nameParts[i+2]][var]
    }
    else
    {
         currentBuild.result = 'ABORTED'
         error('${env.JOB_BASE_NAME} is a invalid jobname, should be named as [Test_]Build_ATP_PLATFORM_ENV')
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
        def ENV = getVars('env')
        def TARGET = getVars('tgt')
        def CPU_X =  toCPU(TARGET)
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
                        def repos = ['atc', 'atp_bhp', 'atp_core']
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

        //Run the Make command to build the project and put the output in a build_log.txt file.
        stage('Build')
        {
            steps
            {
                dir('build/aos_bhp/atp_bhp/impl')
                {
                    sh "make -j4 TARGET=${TARGET} ENV=${ENV} PROJS_ROOT=${workspace}/build |& tee ../../../build_log.txt"

                    // Temporary fix for building without LSSD:
                    sh "cp -p -r ../target ../target_with_lssd"
                    sh "make -j4 TARGET=${TARGET} ENV=${ENV} PROJS_ROOT=${workspace}/build clean"
                    sh "make -j4 TARGET=${TARGET} ENV=${ENV} PROJS_ROOT=${workspace}/build EXTERNAL_DEFINES=-D_SYS_INT"
                }
            }
        }

        stage('Check Build')
        {
            steps
            {
                script
                {
                    //HTML Code for better viewability on the Jenkins Webpage
                    //We pretty much just scan the build_log.txt file for the occurances of "warning: "
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
                sh "zip -R target_ATP_${TARGET}_${ENV} aos_bhp/atp_bhp/target/${CPU_X}/${envi}/atp_bhp* "
                archiveArtifacts artifacts: "target_ATP_${TARGET}_${ENV}.zip", fingerprint: true

                // Temporary fix for building without LSSD:
                sh "zip -R target_with_lssd_ATP_${TARGET}_${ENV} aos_bhp/atp_bhp/target_with_lssd/${CPU_X}/${envi}/atp_bhp* "
                archiveArtifacts artifacts: "target_with_lssd_ATP_${TARGET}_${ENV}.zip", fingerprint: true
            }
        }

        cleanup
        {
            sh 'rm -rf build'
        }
    }
}
