pipeline{
    agent any
    triggers {pollSCM ('* * * * *')}
    // Wipe the workspace so we are building completely clean
    deleteDir()
    stages {
        stage ("Checkout") {
            steps {
                git url: 'https://github.com/timezone4/eventmanager.git'
            }
        }
        stage ("Compile") {
            steps {
                sh "make"
            }
        }
        stage ("Unit test") {
            steps {
                sh "make tests"
            }
        }
 	stage ("Release") {
            steps {
                sh "make rel && cp _rel/*/*.tar.gz /releases" 
            }
        }
    }
    
    
}
