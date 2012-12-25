Alisa
=====

Cute and kindhearted IRC bot.

![Alisa Bosconovitch](http://fc06.deviantart.net/fs71/f/2012/063/c/f/street_fighter_x_tekken_alisa_by_steepgirl-d4rpc7j.png)

What can it do?
----------------

* Find URLs in messages and send brief info about them (title for HTML, content
  type and size for rest)

* Log chat to Lucene and search it via chat or web

* Hug

How can I build it?
-------------------

You need JDK7 and sbt (Scala Build Tool).

		sbt onejar

This will produce one big JAR with all dependencies in
`target/alisa-VERSION.jar`.

How can I run it?
-----------------

You need JRE7.

* Run with test config

		java -jar target/alisa-VERSION.jar

* Run with other config

		java -jar target/alisa-VERSION.jar alisa.conf

* Run with verbose logging

		java -jar target/alisa-VERSION.jar -d

* Run as daemon via attached systemd service file

TODO
----

* Better modularity (global/network/channel)

