# mod_offline_post_log 
Logs offline messages to an HTTP API in json format.


## CONFIGURATION

Add the module to your ejabberd.yml, in the modules section:

```yaml
modules:
  ...
   mod_offline_post_log:
     url: "http://localhost/api"
     username: "basic-auth-username"
     password: "basic-auth-password"
```

## Content-body

```
{  
   "from":{  
         "username":"Alice",
         "server":"localhost",
         "resource":"123456"
          },
   "to":{  
         "username":"Bob",
         "server":"localhost",
         "resource":"789123456789"
        },
   "message":"someone somewhere somewhat"
 }
 ```

## Thanks

This plugin rips off many other already developed plugins, mostly mod_post_log ->
https://github.com/processone/ejabberd-contrib/tree/master/mod_post_log
