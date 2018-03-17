# RabbitMQ 
Rabbitm management plugin [link](http://the.host.to.run.on:15672).

Username: kryo

Password: password

## Sample Config for Adding a Java Worker Instance
- For new instances add queues like `queries-java-n` in the "queues" tab.
- You now need to bind the new queue to the exchange:
  - Click on "bindings"
  - In add bindings write "queries-java" into the "From exchange: " field
  - Click Bind
  
# Setting up the service


