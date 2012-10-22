clicktochat_ejabberd
====================

Click To Chat implemented on Ejabberd

Before To Start
====

You must install Ejabberd and stop it!!

How To Start
====

Clone the project from github:

			$ git clone https://github.com/jorgegarrido/clicktochat_ejabberd.git

Move into project directory:

			$ cd clicktochat_ejabberd

Configure the file 'clicktochat.conf' correclty (each section in the file is described on it)

Let's compile and install:

			$ make && sudo make install

Run 

Registering client users
====

NOTE: At this step you must have created helpdesk users

Let's create a single user, using http interface:

			$ curl -i -H "Accept: text/xml" -X POST -d "<?xml version='1.0'?><registration><username>my_client</username><password>123</password></registration>" http://IP:PORT/clicktochat/register

The response will be an xml containing the client's nick and the 
helpdesk user that is automatically linked to start the chat, from your
application you must send messages to this nick (helpdesk user).

Listing all users connected
====

Using http interface:

			curl -i -X GET http://IP:PORT/clicktochat/list

The response will be an xml containing all helpdesk users connected, now from your 
application you can choose one and start the chat.

