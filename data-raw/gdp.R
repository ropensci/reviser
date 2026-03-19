## code to prepare `gdp` dataset goes here

vintage_start <- as.Date("2002-10-01")
vintage_end <- as.Date("2024-10-01")
period_start <- as.Date("1980-01-01")
period_end <- as.Date("2024-10-01")

gdp_ch <- readxl::read_excel(
  "inst/exdata/realtime_database.xlsx",
  sheet = "gdp",
  skip = 10
) |>
  tidyr::pivot_longer(
    cols = -c(time),
    names_to = "pub_date",
    values_to = "value"
  ) |>
  dplyr::mutate(
    pub_date = zoo::as.Date(zoo::as.yearqtr(
      pub_date,
      format = "%Yq%q"
    )),
    id = "CHE"
  ) |>
  na.omit()

gdp_us <- readxl::read_excel(
  "inst/exdata/realtime_database.xlsx",
  sheet = "gdp_us",
  skip = 10
) |>
  tidyr::pivot_longer(
    cols = -c(time),
    names_to = "pub_date",
    values_to = "value"
  ) |>
  dplyr::mutate(
    pub_date = zoo::as.Date(zoo::as.yearqtr(
      pub_date,
      format = "%Yq%q"
    )),
    id = "US"
  ) |>
  na.omit()

gdp_jp <- readxl::read_excel(
  "inst/exdata/realtime_database.xlsx",
  sheet = "gdp_jp",
  skip = 10
) |>
  tidyr::pivot_longer(
    cols = -c(time),
    names_to = "pub_date",
    values_to = "value"
  ) |>
  dplyr::mutate(
    pub_date = zoo::as.Date(zoo::as.yearqtr(
      pub_date,
      format = "%Yq%q"
    )),
    id = "JP"
  ) |>
  na.omit()

gdp_ea <- readxl::read_excel(
  "inst/exdata/realtime_database.xlsx",
  sheet = "gdp_ea",
  skip = 10
) |>
  tidyr::pivot_longer(
    cols = -c(time),
    names_to = "pub_date",
    values_to = "value"
  ) |>
  dplyr::mutate(
    pub_date = zoo::as.Date(zoo::as.yearqtr(
      pub_date,
      format = "%Yq%q"
    )),
    id = "EA"
  ) |>
  na.omit()

gdp <- dplyr::bind_rows(
  gdp_ch,
  gdp_us
) |>
  dplyr::bind_rows(gdp_jp) |>
  dplyr::bind_rows(gdp_ea) |>
  dplyr::filter(
    pub_date >= vintage_start & pub_date <= vintage_end,
    time >= period_start & time <= period_end
  ) |>
  dplyr::mutate(
    time = as.Date(time)
  )

usethis::use_data(gdp, overwrite = TRUE)
