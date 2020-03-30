emqx_extension_hook
========

An EMQ X plugin

##### emqx_extension_hook.conf

```properties
exhook.rule.client.connected.1     = {"action": "on_client_connected"}
exhook.rule.client.disconnected.1  = {"action": "on_client_disconnected"}
exhook.rule.client.subscribe.1     = {"action": "on_client_subscribe"}
exhook.rule.client.unsubscribe.1   = {"action": "on_client_unsubscribe"}
exhook.rule.session.subscribed.1   = {"action": "on_session_subscribed"}
exhook.rule.session.unsubscribed.1 = {"action": "on_session_unsubscribed"}
exhook.rule.message.publish.1      = {"action": "on_message_publish"}
exhook.rule.message.delivered.1    = {"action": "on_message_delivered"}
exhook.rule.message.acked.1        = {"action": "on_message_acked"}
```

License
-------

Apache License Version 2.0

Author
------

Contributors
------------

