rem
rem When you are using the PolyORB version you need to set this environment variable
rem in each of the clients.
rem There is an IP adress in this string, which needs to correspond to the IP-Adress of your 
rem computer where your server is running.
rem 
rem On the server you need to change in a file called  'polyorb.conf' 
rem look for the attribute: polyorb.protocols.iiop.default_addr
rem adjust this to be the IP-adress of the server.
rem
rem 

set POLYORB_DSA_NAME_SERVICE=corbaloc:iiop:1.2@10.0.1.22:2809/_NameService


