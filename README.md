# omni-check-selenium

Check availability of reference app using selenium
## Assumptions
 - Selenium webdriver is running (java -jar selenium-standalone...)

 - Config parameters should be in src/Config.hs. Config.hs is based on Config.hs.template

## Scenario
Open ump page
Login u1 and u2
sends message to each other
checks that message has been received
