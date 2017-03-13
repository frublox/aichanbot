# AiChanBot

A simple Twitch bot that implements rate-limiting, message-queueing, and a couple of commands (for the moment).

To do:
- Add command cooldowns
- Rewrite functions to do things in `StateT BotState`, rather than manually reading/updating state through `StatefulIRC`
