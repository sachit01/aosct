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

    if (nameParts[i] == 'Lint'
    &&  nameParts[i+1] == 'Dispatcher'
    &&  ['VSIM','EMD','HIL'].contains(nameParts[i+2]))
    {
        return nameParts[i+2]
    }
    else
    {
         currentBuild.result = 'ABORTED'
         error('${env.JOB_BASE_NAME} is a invalid jobname, should be named as [Test_]Lint_Dispatcher_ENV')
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
        def ENV = getEnvVar()
        def TARGET = "PPC"
        def OUT_NAME = "lint_dispatcher_${ENV}"
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
                        def repos = ['aos_bhp', 'atc', 'atp_core', 'dispatcher']
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

        stage('Lint')
        {
            steps
            {
                dir('build/aos_bhp/dispatcher/impl')
                {
                    sh "make -j4 lint TARGET=${env.TARGET} ENV=${env.ENV} PROJS_ROOT=${workspace}/build |& tee ../../../${env.OUT_NAME}_warnings.txt"
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
        }

        cleanup
        {
            sh "rm -rf build"
        }
    }
}
