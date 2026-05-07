# 0.1.0.0

- Initial release. Provides `Analytics.handler`, `Analytics.silentHandler`,
  `Analytics.Settings`, and `Analytics.decoder`. Sync HTTP POST delivery
  to a configured events service URL with bearer auth, bounded timeout,
  and log-and-drop on failure. Wire format is provisional pending the
  events service contract.
