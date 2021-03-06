
	mod_http_upload - HTTP File Upload

	Author: Holger Weiss <holger@zedat.fu-berlin.de>
	Requirements: ejabberd 13.06 or newer


	DESCRIPTION
	-----------

This module allows for requesting permissions to upload a file via HTTP.
If the request is accepted, the client receives a URL to use for uploading
the file and another URL from which that file can later be downloaded.

PLEASE NOTE: This module implements an experimental protocol which has NOT
been approved by the XMPP Standards Foundation and may change at any time:

  http://xmpp.org/extensions/inbox/http-upload.html

There are already suggestions for improvements, e.g. from ProcessOne:

  https://github.com/processone/ejabberd-saas-docs/blob/master/xmpp-specs/http-filetransfer/http-filetransfer.md


	CONFIGURATION
	-------------

In order to use this module, add configuration snippets such as the
following to your ejabberd.yml file:

  listen:
    # [...]
    -
      module: ejabberd_http
      port: 5443
      tls: true
      certfile: "/etc/ejabberd/example.com.pem"
      request_handlers:
        "": mod_http_upload

  modules:
    # [...]
    mod_http_upload:
      docroot: "/home/xmpp/upload"

The configurable mod_http_upload options are:

- host (default: "upload.@HOST@")

  This option defines the JID for the HTTP upload service.  The keyword
  @HOST@ is replaced with the virtual host name.

- name (default: "HTTP File Upload")

  This option defines the Service Discovery name for the HTTP upload
  service.

- access (default: 'local')

  This option defines the access rule to limit who is permitted to use the
  HTTP upload service.  The default value is 'local'.  If no access rule of
  that name exists, no user will be allowed to use the service.

- max_size (default: 104857600)

  This option limits the acceptable file size.  Either a number of bytes
  (larger than zero) or 'infinity' must be specified.

- docroot (default: 'undefined')

  Uploaded files are stored below the directory specified (as an absolute
  path) with this option.  It is mandatory to specify either this option or
  the 'service_url' option.

- put_url (default: "https://@HOST@:5443")

  This option specifies the initial part of the PUT URLs used for file
  uploads.  Note that @HOST@ can NOT be specified for this option in the
  configuration file, but the virtual host name is used as part of the URL
  by default.

- get_url (default: $put_url)

  This option specifies the initial part of the GET URLs used for
  downloading the files.  By default, it is set to the same value as the
  'put_url', but you can set it to a different value in order to have the
  files served by a proper HTTP server such as Nginx or Apache.

- service_url (default: 'undefined')

  If a 'service_url' is specified, HTTP upload slot requests are forwarded
  to this external service instead of being handled by mod_http_upload
  itself.  An HTTP GET query such as the following is issued whenever an
  HTTP upload slot request is accepted as per the 'access' rule:

    http://localhost:5444/?jid=juliet%40example.com&size=10240&name=example.jpg

  In order to accept the request, the service must return an HTTP status
  code of 200 or 201 and two lines of text/plain output.  The first line is
  forwarded to the XMPP client as the HTTP upload PUT URL, the second line
  as the GET URL.

  In order to reject the request, the service should return one of the
  following HTTP status codes:

  - 402
    In this case, a 'resource-constraint' error stanza is sent to the
    client.  Use this to indicate a temporary error after the client
    exceeded a quota, for example.

  - 403
    In this case, a 'not-allowed' error stanza is sent to the client.  Use
    this to indicate a permanent error to a client that is not permitted to
    upload files, for example.

  - 413
    In this case, a 'not-acceptable' error stanza is sent to the client.
    Use this if the file size was too large, for example.

  In any other case, a 'service-unavailable' error stanza is sent to the
  client.
