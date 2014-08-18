java \
-Xmx1024M -Xss2M -XX:PermSize=64M -XX:MaxPermSize=384M\
-XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch-0.12.1.jar "$@"
