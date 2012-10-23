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

Enable Click To Chat
====

In the config file /etc/ejabberd/ejabberd.cfg configure the sections:

modules, add mod_clicktochat:

			{modules,
		         [...
          		  ...
          		  ...
          		  {mod_clicktochat, []}
         		 ]}.

and in the section ejabberd_http, add a new handler mod_http_clicktochat

			{5280, ejabberd_http, [...
                                   ...
                                   http_poll,
                                   %%register,
                                   web_admin,
                                   {request_handlers,
                                    [ {["clicktochat"], mod_http_clicktochat}]}
                                  ]}.


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

Request - Response RESTful Services Description
====

	-----------------------------------------------------------------------------------------------------------------
	|         URI             | Method |           Request Body             |              Response                 |
	-----------------------------------------------------------------------------------------------------------------
	|                         |        |   <?xml version='1.0'?>            |     <?xml version='1.0'?>             |	
	|  /clicktochat/register  |  POST  |   <registration>                   |     <register>                        |
	|                         |        |     <username>username</username>  |       <status>ok</status>             |
	|                         |        |     <password>pass</password>      |       <from>user_from@domain</from>   |
	|                         |        |   </registration>                  |       <to>user_to@domain</to>         |
	|                         |        |                                    |     </register>                       |
	-----------------------------------------------------------------------------------------------------------------
	|                         |        |                                    |     <?xml version='1.0'?>             |
	|  /clicktochat/list      |  GET   |              empty                 |     <connected_users>                 |
	|                         |        |                                    |       <username>username</username>   |
	|                         |        |                                    |     </connected_users>                |
	-----------------------------------------------------------------------------------------------------------------	
		

Round Robin Queue
====

This project implements the round robin in the queue, when a client user is registered
and connected, then in the queue, a helpdesk user is get and placed to the end
of the queue, ensuring that the client users are equally asigned to the helpdesk users,
avoiding that a single user attends all requests.

Integrating with Riak
====


