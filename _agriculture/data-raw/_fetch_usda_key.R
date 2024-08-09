# fetch usda_key value----
## you'll need an API key to use USDA data (https://quickstats.nass.usda.gov/api)
## once you have the API key, set it using
## keyring::key_set("usda_key")
## entering the key in the pop-up window
usda_key <- keyring::key_get("usda_key")
cli::cli_alert_success("USDA API key loaded")
