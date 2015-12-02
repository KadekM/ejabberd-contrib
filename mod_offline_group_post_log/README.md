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
  "room": {
    "jid": "testroom@conference.localhost",
    "users": {
      "subscribed": ["foo@localhost", "foo2@localhost"],
      "online": ["foo@localhost"]
    }
  },
  "from": {
    "jid": "marek@localhost",
    "nick": "marecek"
    }
   "message": "hello world",
   "timestamp": "123",
   "xurls": ["url1", "url2"]
}
 ```

## Thanks

This plugin rips off many other already developed plugins, mostly mod_post_log ->
https://github.com/processone/ejabberd-contrib/tree/master/mod_post_log
