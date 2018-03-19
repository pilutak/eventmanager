pipeline{
    agent any
    triggers {pollSCM ('* * * * *')}
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
        stage ("Dialyze") {
            steps {
                sh "make dialyze"
            }
        }
    }
    
    
}
