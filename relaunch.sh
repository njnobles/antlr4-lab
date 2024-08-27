while true
do
  java -cp ~/.m2/repository/org/antlr/antlr4-lab/0.3-SNAPSHOT/antlr4-lab-0.3-SNAPSHOT-complete.jar org.antlr.v4.server.ANTLRHttpServer
  cp log/antlrlab/antlrlab.log log/antlrlab/antlrlab-died.log
  sleep 1
done
