#####################
# INITIAL CONTAINER #
#     Run tests     #
#####################
FROM maven:3.6.3-jdk-14 AS builder

# Set-up working directory
ENV HOME=/home/usr/app
RUN mkdir -p $HOME
WORKDIR $HOME

# Lets Docker use a cache so it doesn't install the internet
ADD pom.xml $HOME
RUN ["/usr/local/bin/mvn-entrypoint.sh", "mvn", "verify", "clean", "--fail-never"]

# Copy all files from host computer to container
ADD . $HOME

# Run specified test suites
RUN mvn test -Dsurefire.suiteXmlFiles=tests_functional-smoke.xml
RUN mvn test -Dsurefile.suiteXmlFiles=tests_oracle.xml
RUN mvn test -Dsurefire.suiteXmlFiles=tests_functional-data-validation.xml
RUN mvn test -Dsurefire.suiteXmlFiles=tests_functional-data-intensive.xml

# Run performance tests and edit the folder names
RUN ./tests_performance-runner.sh


############################
#  INTERMEDIATE CONTAINER  #
#  Generate Allure reports #
############################
FROM ubuntu:latest AS report-generator

# Set-up working directory
ENV HOME=/home/usr/app
RUN mkdir -p $HOME
WORKDIR $HOME

# Copy report logs over from the previous container
COPY --from=builder /home/usr/app/target/allure-results/ /allure-results/

# Needed for Allure
RUN apt-get update && apt-get install -y curl tar

# Needed for Java
RUN apt-get update && \
    apt-get install -y openjdk-8-jdk && \
    apt-get install -y ant && \
    apt-get clean;
RUN apt-get update && \
    apt-get install ca-certificates-java && \
    apt-get clean && \
    update-ca-certificates -f;
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/
RUN export JAVA_HOME

# Download and install Allure
RUN curl -o allure-2.6.0.tgz -Ls https://dl.bintray.com/qameta/generic/io/qameta/allure/allure/2.6.0/allure-2.6.0.tgz && \
    tar -zxvf allure-2.6.0.tgz -C /opt/ && \
    ln -s /opt/allure-2.6.0/bin/allure /usr/bin/allure && \
    allure --version

# Generate reports from the logs
RUN allure generate /allure-results/ --clean -o allure/


###################
# FINAL CONTAINER #
#  Serve reports  #
###################
FROM nginx

# Get the files we want to serve including the nginx config file
COPY --from=report-generator /home/usr/app/allure/ /usr/share/nginx/html/
COPY --from=builder /home/usr/app/target/gatling/ /usr/share/nginx/html/gatling
COPY --from=builder /home/usr/app/conf.d /etc/nginx/conf.d

# Expose http and https of nginx web server
EXPOSE 80 443
CMD ["nginx", "-g", "daemon off;"]