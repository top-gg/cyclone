# Cyclone

<div align="center">
<img  src="https://cdn.discordapp.com/avatars/890619422413307965/ddd1f103e9b70398953fcd680f53478d.png?size=256" />

<sub align="center">Target one cloned/forked bot in the server; destroy that bot.</sub>

</div>

<br />

A pattern matching framework for detecting lazy clones of popular open source bots with 0 or very few modifications.

## Declaring patterns

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
My commands are: `slots`, `blackjack`, `poker`
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
- `matcher`: `EmbedMatcher | MessageMatcher` - Either an embed or message matcher in the same object

### `EmbedMatcher`

- `type`: `"embed"` - Define the type of the matcher
- `title`: `pattern?` - An title to match the embed against. Supports custom pattern syntax.
- `description`: `pattern?` - A description to match the embed against. Supports custom pattern syntax.
- `footer`: `pattern?` - A footer to match the embed against. Supports custom pattern syntax.

### `MessageMatcher`

- `type`: `"message"` - Define the type of the matcher
- `content`: `pattern?` - Message content to match against. Supports custom pattern syntax.

### `Default`

Dynamic key-value pairs representing a variable name and value. Used for comparing a matching pattern against the known defaults of a bot.

Must be an array if values are found inside a `@loop`

```yaml
embed:
  title: "{emoji} Welcome: **{botName: ?}**"
  description: "Some specific message here"
defaults:
  botName: Open Source Bot
```

## Setting up

Install the Haskell toolchain from https://www.haskell.org/ghcup/

To test `cabal test`

To run the bot `CYCLONE_TOKEN=your-token-here cabal run cyclone`

## Why Haskell?

Cyclone's job is parsing the structure of messages (most notably help messages) sent by bots. They often tend to involve a specific loop construct or recursion which either cannot be modeled by regular expressions at all, or require very complex regexes that are difficult to understand by non-developers and developers alike.

Dealing with this in a flexible way requires creating our own Domain Specific Language (DSL) that is less powerful than regex, but properly models the problem we're trying to solve.

Haskell is known for its expressiveness when it comes to building parsers and compilers. Libraries like [Parsec](https://hackage.haskell.org/package/parsec) and friends make the task of making a parser that generates parsers from a DSL (which is what cyclone does) _much_ simpler than it would be with a language like Javascript. It's also an overall very pleasant language to use.
