server <- function(input, output, session) {
  # Reactive values to hold the tokens
  auth_creds <- reactiveValues(temp_token = NULL, temp_secret = NULL)

  observeEvent(input$start_auth, {
    req_url <- "https://pubmlst.org/bigsdb/rest/db/pubmlst_neisseria_seqdef/oauth/get_request_token"
    myapp <- oauth_app(
      "pubmlst",
      key = "wPA1wtuBb1Q00QlTppQ3oPfr",
      secret = "YbiPpM3wxcFJg5fzdY2qmRQhgYP467ZIsmbV2Z0Lls"
    )

    # Use 'oob' (Out of Band) to tell PubMLST to display a PIN
    sig <- oauth_signature(
      req_url,
      method = "GET",
      app = myapp,
      other_params = list(oauth_callback = "oob")
    )

    # Manual Header construction (the most reliable way)
    auth_header <- paste0(
      'OAuth ',
      paste(names(sig), paste0('"', sig, '"'), sep = '=', collapse = ', ')
    )

    resp <- GET(req_url, add_headers(Authorization = auth_header))

    if (status_code(resp) == 200) {
      token_data <- fromJSON(content(resp, "text"))
      auth_creds$temp_token <- token_data$oauth_token
      auth_creds$temp_secret <- token_data$oauth_token_secret

      # Show the next step in the UI
      output$step2_ui <- renderUI({
        auth_url <- paste0(
          "https://pubmlst.org/cgi-bin/bigsdb/bigsdb.pl?db=pubmlst_neisseria_seqdef&page=authorizeClient&oauth_token=",
          auth_creds$temp_token
        )
        tagList(
          hr(),
          p("2. Click this link and click 'Authorize':"),
          a(
            "Open PubMLST Authorization",
            href = auth_url,
            target = "_blank",
            class = "btn btn-info"
          ),
          p("3. Paste the PIN code you receive below:"),
          textInput("verifier_pin", "Verifier PIN"),
          actionButton("finalize_auth", "Link Account")
        )
      })
    }
  })
}
