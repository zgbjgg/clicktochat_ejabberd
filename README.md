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

Let's create a single user, using a http interface:

			$ curl -i -H "Accept: text/xml" -X POST -d "<?xml version='1.0'?><registration><username>george5</username><password>123</password></registration>" http://192.168.24.182:5280/clicktochat/register