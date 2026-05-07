# nri-analytics

Sync HTTP delivery of typed analytics events to the NoRedInk events service.

This package is consumed by NoRedInk's monolith. It produces an
`AnalyticsHandler` whose `sendEvent` is wired into `nri-prelude`'s
`Platform.Analytics.Internal` callback at the application root. See the
companion design doc in `NoRedInk/event-platform/docs`.
