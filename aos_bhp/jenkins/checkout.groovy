def checkoutRepo(repoName, name)
{
    dir("${repoName}")
    {
        echo "Checking out ${repoName} ${name}"

        return checkout([ scm: [$class: 'GitSCM', name: "${repoName}", branches: [[name: name ]],
            extensions: [[$class: 'CloneOption', timeout: 20]],
            userRemoteConfigs: [[ refspec: "+refs/heads/*:refs/remotes/origin/* +refs/changes/*:refs/remotes/origin/changes/*",
                                  url: "ssh://10.160.155.37:29418/${repoName}", credentialsId: 'ed5f023f-54c8-41f2-b794-cc0dc5d96a51']] ]]).GIT_COMMIT
    }
}

pipeline
{
    agent { label 'git' }

    options
    {
        buildDiscarder(logRotator(daysToKeepStr: '5'))

        disableConcurrentBuilds()

        // If not set, jenkins will try to checkout the
        // master repo by itself. Which we don't want, as
        // we manually clone in a later step.
        skipDefaultCheckout true
    }

    stages
    {
        stage('Checkout')
        {
            steps
            {
                script
                {
                    if (!(params.git_aos_bhp && params.git_atc && params.git_atp_bhp && params.git_atp_core && params.git_dispatcher && params.git_dmi_bhp && params.git_tools_bhp))
                    {
                        currentBuild.result = 'ABORTED'
                        error('No git commit specified, aborting')
                    }

                    dir('.')
                    {
                        checkoutRepo('aos_bhp',    params.git_aos_bhp)
                        checkoutRepo('atc',        params.git_atc)
                        checkoutRepo('atp_bhp',    params.git_atp_bhp)
                        checkoutRepo('atp_core',   params.git_atp_core)
                        checkoutRepo('dispatcher', params.git_dispatcher)
                        checkoutRepo('dmi_bhp',    params.git_dmi_bhp)
                        checkoutRepo('tools_bhp',  params.git_tools_bhp)
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
            dir('.')
            {
                script
                {
                    def repos = ['aos_bhp', 'atc', 'atp_bhp', 'atp_core', 'dispatcher', 'dmi_bhp', 'tools_bhp']

                    for (repo in repos)
                    {
                        sh "tar --create --file ${repo}.tar.gz --exclude=.git\\* --gzip ${repo}"
                        archiveArtifacts artifacts: "${repo}.tar.gz", fingerprint: true
                    }
                }
            }
        }
    }
}
