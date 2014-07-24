Alisa
=====

Cute and kindhearted IRC bot.

![Alisa Bosconovitch](http://fc06.deviantart.net/fs71/f/2012/063/c/f/street_fighter_x_tekken_alisa_by_steepgirl-d4rpc7j.png)

What can it do?
----------------

(`alisa` denotes current bot's nick)

* Find URLs in messages and send brief info about them (title for HTML, content
  type and size for rest)

* Log chat to Lucene and search it via chat or web. Say `alisa: log` for help.

* Show recently played Last.fm track. Say `alisa: lf`. If your IRC ~username
  (not nick!) is different from your Last.fm username you have to pair it by
  saying `alisa: lf your_lastfm_username`. You have to do this only once. You
  can also append integer offset to show n-th recent track. `np` is alias for
  `lf`.

* Hug

How can I build it?
-------------------

You need JDK8 and sbt (Scala Build Tool).

		sbt onejar

This will produce one big JAR with all dependencies in
`target/alisa-VERSION.jar`.

How can I run it?
-----------------

You need JRE8.

* Run with test `alisa.conf` in current directory

		java -jar target/alisa-VERSION.jar

* Run with other config

		java -jar target/alisa-VERSION.jar /etc/alisa.conf

* Run with verbose logging

		java -jar target/alisa-VERSION.jar -d

* Run as daemon via systemd service file

	Do following as root:

		useradd -rU alisa
		mkdir /var/lib/alisa
		chown alisa:alisa /var/lib/alisa
		chmod 700 /var/lib/alisa
		cp target/alisa-VERSION.jar /usr/local/share/alisa.jar
		cp alisa.conf /etc
		chgrp alisa /etc/alisa.conf

		cp alisa.service /etc/systemd/system
		systemctl --system daemon-reload
		systemctl start alisa.service

* Run as daemon via LSB init script (Debian example)

	You also need bash, logger and start-stop-daemon. Do following as root:

		useradd -rU alisa
		mkdir /var/lib/alisa
		chown alisa:alisa /var/lib/alisa
		chmod 700 /var/lib/alisa
		cp target/alisa-VERSION.jar /usr/local/share/alisa.jar
		cp alisa.conf /etc
		chgrp alisa /etc/alisa.conf

		cp init /etc/init.d/alisa

		# on Debian
		update-rc.d alisa defaults

		# on CentOS (untested)
		#chkconfig --add alisa

		# on distro with lsb-core (untested)
		#/usr/lib/lsb/install_initd /etc/init.d/alisa

		/etc/init.d/alisa start

TODO
----

* Better modularity (global/network/channel)
* log: Log into PostgreSQL, Lucene is huge and it's not that much useful here
* network: Fix reconnecting already ffs
* reply module

THANKS
------
I was inspired by
* shell clan (#sh.nezuiz @ QuakeNet)
* MikuBot by [nipah](https://vocadb.net/Profile/riipah)
* [tewibot](https://github.com/neeee/tewibot)
