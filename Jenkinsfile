pipeline{
    agent any
    triggers {pollSCM ('* * * * *')}
    stages {
        stage ("Checkout") {
            // Wipe the workspace so we are building completely clean
            deleteDir()
            
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
