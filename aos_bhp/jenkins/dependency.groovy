/* This job should produce a zip file
 * containing the dependencies needed
 * to build everything.
 *
 * The following files should be exported:
 *  *SPL_Delivery
 *  *SetTimeOfDay
 *  *TimeSyncServer
 */
pipeline
{
    agent any

    options
    {
        buildDiscarder(logRotator(daysToKeepStr: '5'))

        // If not set, jenkins will try to checkout the
        // master repo by itself. Which we don't want, as
        // we manually clone in a later step.
        skipDefaultCheckout true
    }

    stages
    {
        stage('Pack dependencies')
        {
            steps {
                dir('tmp'){
                    sh 'cp -r ~/dependencies/* ./'
                    zip zipFile: 'dependencies.zip', archive:true
                }
            }
        }
    }

    post
    {
        cleanup
        {
            dir('tmp')
            {
                deleteDir()
            }
        }
    }
}
