### Removed

- `!!` operator. Use `?` instead — the two operators were identical aliases since
  introduction, and removing one eliminates a stylistic split that added review
  cost without benefit. Sources using `!!` will fail to parse; replace each `!!`
  with `?`. (#1568)
