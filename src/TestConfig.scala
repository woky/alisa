package alisa


object TestConfig extends Config(
	networks = Seq(
		NetworkConfig(
			name = "quakenet",
			nick = "alisa-test",
			finger = "http://tekken.wikia.com/wiki/Alisa_Bosconovitch",
			servers = Seq(
				ServerConfig("irc.quakenet.org")
			),
			channels = Seq(
				ChannelConfig("#alisa-test")
			)
		)
	),
	modules = List(
		Jetty(),
		Lucene("/tmp/alisa-test"),
		Cointoss,
		Logs(),
        Love(),
		UrlInfo()
	)
)
