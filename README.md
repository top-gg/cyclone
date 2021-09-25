# Cyclone

<div align="center">
<img  src="https://cdn.discordapp.com/avatars/890619422413307965/ddd1f103e9b70398953fcd680f53478d.png?size=256" />

<sub align="center">Target one cloned/forked bot in the server; destroy that bot.</sub>

</div>

<br />

A pattern matching framework for detecting lazy clones of popular open source bots with 0 or very few modifications.

## Declaring patterns

_WIP_

Adding new patterns is as simple as creating a new yaml file under `/detections`

```yaml
name: Jisoo Bot
detections:
  - name: Identical help command
    # probably not needed but why not
    input: "{prefix}help"
    embed:
      title: Welcome to my help menu, here are my commands
      # custom language for matching bot responses
      description: |
        @loop
        {emoji} **{category: ?}** {emoji}
        {commands: @list(",", "`?`")}
        @loop
    defaults:
      - category: MODERATION
        commands: ["ban", "kick", "mute"]
      - category: MUSIC
        commands: ["play", "volume"]
```

## Custom Syntax

Special characters are enclosed in either `{}` or begin with a `@`

### Wildcards

The `{?}` token will match characters non-greedily until it can match the rest of the pattern against the input.

Wildcard patterns can be given a name with `{name: ?}`

### Emojis

The `{emoji}` token will match any emoji in the format of `:emoji_name:` or Discord's `<:emoji_name:1234567>`

Emojis can be given a name with `{emoji_label: emoji}`

### Lists

_WIP_

`@list(delimiter, pattern)` function matches text that is separated by a `delimiter` like `,`. Useful in matching a list of commands in the form of

```
My commands are: `theme_park`, `blackjack`, `hookers`
```

with the pattern

```
My commands are: {@list(",", "`?`")}
```

You can use a question mark without braces to represent a wildcard inside list patterns.

### Loops

Patterns inside `@loop` will be matched **at least** once inside a loop. Extra whitespace in the beginning and the end of the loop are ignored.

```
@loop
`{?}` -> {?}
@loop
```

will match message content like

```
`Moderation` -> test1, test2
`Music` -> test3, test4
`So on` -> and so, forth
```

## Markup API

### `BotConfig`

- `name`: `string?` - Name of the bot that will be detected
- `platform`: `string?` - The platform this detection applies to (Discord by default)
- `link`: `string` - Link to the github repo of the bot
- `detections`: `Detection[]` - An array of detections

### `Detection`

- `name`: `string` - A unique name for the detection
- `input`: `string?` - The command that that triggers the detection. (For example `{prefix}help`)
- `defaults`: `Default? | Default[]?` - Values that describe the variables for what the original bot sends in its messages.
- `*`: `EmbedMatcher | MessageMatcher` - Either an embed or message matcher in the same object

### `EmbedMatcher`

- `title`: `pattern?` - An title to match the embed against. Supports custom pattern syntax.
- `description`: `pattern?` - A description to match the embed against. Supports custom pattern syntax.
- `footer`: `pattern?` - A footer to match the embed against. Supports custom pattern syntax.

### `MessageMatcher`

- `content`: `pattern?` - Message content to match against. Supports custom pattern syntax.

### `Default`

Dynamic key-value pairs representing a variable name and value. Used for comparing a matching pattern against the known defaults of a bot.

Must be an array if values are found inside a `@loop`

```yaml
embed:
  title: "{emoji} Welcome: **{bot_name: ?}**"
  description: "Some specific message here"
defaults:
  bot_name: Open Source Bot
```
