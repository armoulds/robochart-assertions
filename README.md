# RoboChart Assertions Editor [![CI](https://github.com/UoY-RoboStar/robochart-assertions/actions/workflows/main.yml/badge.svg?branch=master)](https://github.com/UoY-RoboStar/robochart-assertions/actions/workflows/main.yml)
This project includes the plugins that provide the textual editor for RoboChart Assertions.

### Development platform requirements ###

* Eclipse 2021-12
* Xtext 2.25.0 can be found in https://download.eclipse.org/releases/2021-12 under Modeling
* Maven
* Git

### Build (maven) ###

        1. mvn clean install

### Build (eclipse) ###

        1. Right click circus.robocalc.robochart.assertions/src/circus.robocalc.robochart.assertions/GenerateAssertions.mwe2
            1. select 'Run As' > 'MWE2 Workflow'

### Run (eclipse) ###

        1. Right click circus.robocalc.robochart.assertions.parent
            1. select 'Run As'
            2. double click 'Eclipse Application'
        2. Select the new configuration
            1. click 'Run'
            
### Protocol for updating the tool ###

Whenever updating the tool, follow these steps:

        1. Run Maven with Tests
        2. Change the language reference manual
        3. Change the tool manual

If changes to documentations are not possible immediately, create issues indicating exactly what needs to be done.
