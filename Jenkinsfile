pipeline{
    agent any
    triggers {pollSCM ('* * * * *')}
    stages {
        stage ("Checkout") {
            steps {
            // Wipe the workspace so we are building completely clean
            deleteDir()
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
