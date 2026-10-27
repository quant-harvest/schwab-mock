FROM eclipse-temurin:21-jre-alpine
WORKDIR /app
COPY target/scala-3.3.6/schwab-mock-assembly-*.jar /app/schwab-mock.jar
EXPOSE 8095
ENV PORT=8095
ENTRYPOINT ["java", "-Xmx512m", "-jar", "/app/schwab-mock.jar"]
