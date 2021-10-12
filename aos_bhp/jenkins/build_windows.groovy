/************************************************************
*            DO NOT CHANGE THE NAME OF THIS JOB             *
*************************************************************/

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
                        def repos = ['atc', 'atp_bhp', 'atp_core', 'dispatcher', 'dmi_bhp', 'tools_bhp']
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
                dir('build/aos_bhp/atp_bhp/nvshft/implementation')
                {
                    sh "make"
                    sh "make TARGET=WIN32"
                }

                dir('build/aos_bhp/atp_bhp/target')
                {
                    sh "make PROJS_ROOT=${workspace}/build calc_crc"
                    sh "make PROJS_ROOT=${workspace}/build nvshft"
                    sh "make PROJS_ROOT=${workspace}/build nvshfr"
                }

                dir('build/aos_bhp/atp_bhp/impl')
                {
                    sh "make -j4 TARGET=WIN32 ENV=SIL PROJS_ROOT=${workspace}/build |& tee ../../../build_log.txt"

                    // Temporary fix for building without LSSD:
                    sh "mv ../target/cpu_a/sil/test_atp.exe ../target/cpu_a/sil/test_atp_with_lssd.exe"
                    sh "make TARGET=WIN32 ENV=SIL PROJS_ROOT=${workspace}/build clean"
                    sh "make -j4 TARGET=WIN32 ENV=SIL PROJS_ROOT=${workspace}/build EXTERNAL_DEFINES=-D_SYS_INT"

                    // Build for host and run, to generate the event files:
                    sh "make -j4 TARGET=HOST ENV=SIL PROJS_ROOT=${workspace}/build"
                    sh "${workspace}/build/aos_bhp/atp_bhp/target/sil/atp_bhp_sil"
                    sh "cat ${workspace}/build/aos_bhp/tools_bhp/GenerateEventXML/aos_events.xml.ErrorLog"
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
                            def logwarnings = warnTxt.split('\n')
                            if (logwarnings.size() > 0 && warnTxt.trim() != "")
                            {
                                dir("warnings/${env.JOB_NAME}/")
                                {
                                    writeFile file: 'warnings.txt', text: "${env.JOB_NAME}:${env.BUILD_NUMBER}\n"+warnTxt
                                    def desc = "<p><b>Warnings</b></p>"
                                    for (w in logwarnings)
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
                sh "mkdir -p output"
                sh "cp -p aos_bhp/tools_bhp/AddTrain/AddTrain.bat output"
                sh "cp -p aos_bhp/tools_bhp/AddTrain/AddTrain.docx output"
                sh "cp -p aos_bhp/tools_bhp/AOSManager/AOSRoot/AOSManagerCreateTrain.bat output"

                sh "cp -p -r aos_bhp/tools_bhp/AOSAnalyzer/Distribution output/AOSAnalyzer"

                sh "cp -p -r aos_bhp/tools_bhp/AOSManager/AOSManager/bin/Release output/AOSManager"

                sh "mkdir -p output/AOSEvents"
                sh "cp -p aos_bhp/tools_bhp/GenerateEventXML/AOSEvents.xlsx output/AOSEvents"
                sh "cp -p aos_bhp/tools_bhp/GenerateEventXML/aos_events.xml output/AOSEvents"
                sh "cp -p aos_bhp/tools_bhp/GenerateEventXML/aos_source_types.xml output/AOSEvents"

                sh "mkdir -p output/AOSPC"
                sh "cp -p aos_bhp/tools_bhp/AOSPC/Release/*.exe aos_bhp/tools_bhp/AOSPC/Release/*.dll output/AOSPC"
                sh "cp -p aos_bhp/tools_bhp/AOSPC/Release/*.exe aos_bhp/tools_bhp/AOSPC/Release/*.ini output/AOSPC"
                sh "sed -i 's/^ATPFile[ =\t].*\$/ATPFile = ..\\\\TestATP\\\\test_atp.exe/g' output/AOSPC/AOSPC.ini"

                sh "cp -p -r aos_bhp/dmi_bhp/Implementation/output output/DMI"

                sh "mkdir -p output/NVSH/data_files"
                sh "cp -p aos_bhp/atc/nvshft/calc_crc output/NVSH"
                sh "cp -p aos_bhp/atc/nvshft/nvshft output/NVSH"
                sh "cp -p aos_bhp/atc/nvshft/nvshft_generic output/NVSH"
                sh "cp -p aos_bhp/atc/nvshft/nvshft_add_crc.sh output/NVSH"
                sh "cp -p aos_bhp/atc/nvshfr/nvshfr output/NVSH"
                sh "cp -p /lib64/libstdc++.so.6 output/NVSH"
                sh "cp -p aos_bhp/atp_bhp/nvshft/data_files/*.txt output/NVSH/data_files"
                sh "cp -p -r aos_bhp/atp_bhp/nvshft/release output/NVSH/old_nvshft"

                sh "mkdir -p output/RU"
                sh "cp -p aos_bhp/tools_bhp/RU/RelNotes.txt aos_bhp/tools_bhp/RU/Release/*.* output/RU"

                sh "mkdir -p output/Support"
                sh "cp -p aos_bhp/tools_bhp/AddTrain/CreateShortcut/Release/CreateShortcut.exe output/Support"
                sh "cp -p aos_bhp/tools_bhp/AddTrain/EditIniFile/Release/EditIniFile.exe output/Support"

                sh "cp -p -r aos_bhp/tools_bhp/TCCSim/Distribution output/TCCSim"

                sh "mkdir -p output/TestATP"
                sh "cp -p aos_bhp/atp_bhp/target/cpu_a/sil/* output/TestATP"

                // Temporary fix for building without LSSD:
                sh "mkdir -p output/TestATP_withLSSD"
                sh "cp -p output/TestATP/* output/TestATP_withLSSD"
                sh "rm output/TestATP/test_atp_with_lssd.exe"
                sh "rm output/TestATP_withLSSD/test_atp.exe"
                sh "mv output/TestATP_withLSSD/test_atp_with_lssd.exe output/TestATP_withLSSD/test_atp.exe"

                // TODO - zip each source repo (but only if the version indicates a release?)
            }

            dir('build/output')
            {
                sh "zip -r aos_bhp_win32.zip *"
                archiveArtifacts artifacts: "aos_bhp_win32.zip", fingerprint: true
            }
        }

        cleanup
        {
            sh 'rm -rf build'
        }
    }
}
