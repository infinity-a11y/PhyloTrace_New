box::use(
  shiny[
    NS,
    moduleServer,
    actionButton,
    icon,
    reactiveVal,
    reactiveValues,
    uiOutput,
    renderUI,
    p,
    div,
    span,
    req,
    tags,
    observeEvent,
    textInput,
    showModal,
    hr,
    removeModal,
    modalDialog,
    modalButton
  ],
  bslib[
    page_sidebar,
    sidebar,
    card,
    card_header,
    card_body,
    layout_column_wrap,
    value_box
  ],
)

# =========================================================================
# 1. TIER 1 MODULE: Individual Value Box
# =========================================================================
box_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("box_container"))
}

box_server <- function(id, default_index, remove_box_callback) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    title_text <- reactiveVal(paste("Metric Plot", default_index))
    is_editing <- reactiveVal(FALSE)

    # Edit Toggle Logic
    observeEvent(input$toggle_edit, {
      if (is_editing()) {
        if (nzchar(input$title_input)) {
          title_text(input$title_input)
        }
        is_editing(FALSE)
      } else {
        is_editing(TRUE)
      }
    })

    # Trigger Modal Confirmation for Box Deletion
    observeEvent(input$trigger_delete_box, {
      showModal(modalDialog(
        title = "Delete Value Box",
        paste0(
          "Are you sure you want to permanently delete '",
          title_text(),
          "'?"
        ),
        footer = list(
          actionButton(
            ns("confirm_delete_box"),
            "Delete Element",
            class = "btn-danger"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    # If Confirmed, bubble up the instruction to the Group Parent
    observeEvent(input$confirm_delete_box, {
      removeModal()
      remove_box_callback()
    })

    output$box_container <- renderUI({
      header_content <- if (is_editing()) {
        textInput(
          ns("title_input"),
          label = NULL,
          value = title_text(),
          width = "100%"
        )
      } else {
        title_text()
      }

      btn_icon <- if (is_editing()) icon("check") else icon("pencil")

      value_box(
        title = header_content,
        value = "456",
        theme = "teal",
        p("The 2nd detail"),
        p("The 3rd detail"),
        div(
          style = "position: absolute; top: 10px; right: 10px; z-index: 10; display: flex; gap: 5px;",
          actionButton(
            ns("toggle_edit"),
            label = NULL,
            icon = btn_icon,
            class = "btn-sm btn-light"
          ),
          actionButton(
            ns("trigger_delete_box"),
            label = NULL,
            icon = icon("trash"),
            class = "btn-sm btn-outline-danger"
          )
        )
      )
    })
  })
}

# =========================================================================
# 2. TIER 2 MODULE: Group Container Card
# =========================================================================
group_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("group_card_layout"))
}

group_server <- function(id, assigned_name, remove_group_callback) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    group_name <- reactiveVal(assigned_name)
    created_timestamp <- Sys.time()
    modified_timestamp <- reactiveVal(Sys.time())

    box_counter <- reactiveVal(1)
    active_boxes <- reactiveVal(1)

    touch_group <- function() {
      modified_timestamp(Sys.time())
    }

    # CRITICAL FIX: Setup the initial startup box EXACTLY ONCE
    local({
      initial_box_id <- 1
      box_server(
        id = paste0("box_", initial_box_id),
        default_index = initial_box_id,
        remove_box_callback = function() {
          active_boxes(setdiff(active_boxes(), initial_box_id))
          touch_group()
        }
      )
    })

    # CRITICAL FIX: Instantiate new box servers ONLY when the add button is clicked
    observeEvent(input$add_box_btn, {
      new_id <- box_counter() + 1
      box_counter(new_id)

      local({
        current_id <- new_id
        box_server(
          id = paste0("box_", current_id),
          default_index = current_id,
          remove_box_callback = function() {
            active_boxes(setdiff(active_boxes(), current_id))
            touch_group()
          }
        )
      })

      active_boxes(c(active_boxes(), new_id))
      touch_group()
    })

    # Trigger Group Deletion Modal
    observeEvent(input$trigger_delete_group, {
      showModal(modalDialog(
        title = "⚠️ Delete Whole Group",
        paste0(
          "Warning: This will destroy the group '",
          group_name(),
          "' and all nested plots inside it."
        ),
        footer = list(
          actionButton(
            ns("confirm_delete_group"),
            "Delete Group",
            class = "btn-danger"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    # Confirmed Group Deletion Group Callback
    observeEvent(input$confirm_delete_group, {
      removeModal()
      remove_group_callback()
    })

    # Render Card View Layer
    output$group_card_layout <- renderUI({
      ids <- active_boxes()

      box_uis <- lapply(ids, function(i) {
        box_ui(ns(paste0("box_", i)))
      })

      add_card_placeholder <- value_box(
        title = "Add Element",
        value = "New Box",
        showcase = icon("plus"),
        theme = "secondary",
        p("Append a value block inside this group wrapper."),
        actionButton(
          ns("add_box_btn"),
          "Add Box",
          icon = icon("plus"),
          class = "btn-primary btn-sm w-100"
        )
      )

      all_elements <- c(box_uis, list(add_card_placeholder))

      card(
        card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            span(
              tags$strong(group_name()),
              style = "font-size: 1.25rem; color: #2c3e50;"
            ),
            actionButton(
              ns("trigger_delete_group"),
              "Delete Group",
              icon = icon("folder-minus"),
              class = "btn-sm btn-danger"
            )
          )
        ),
        card_body(
          div(
            style = "font-size: 0.8rem; color: #7f8c8d; margin-bottom: 12px; display: flex; gap: 20px;",
            span(paste(
              "📅 Created:",
              format(created_timestamp, "%Y-%m-%d %H:%M:%S")
            )),
            span(paste(
              "✏️ Last Modified:",
              format(modified_timestamp(), "%Y-%m-%d %H:%M:%S")
            ))
          ),
          layout_column_wrap(
            width = "250px",
            !!!all_elements
          )
        )
      )
    })
  })
}

# =========================================================================
# 3. TIER 3 MODULE: Main Application Engine
# =========================================================================
#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Dashboard Management",
      actionButton(
        ns("trigger_group_modal"),
        "Add New Group",
        icon = icon("folder-plus"),
        class = "btn-success w-100 mb-3"
      ),
      hr(),
      uiOutput(ns("sidebar_navigation"))
    ),
    div(
      style = "display: flex; flex-direction: column; gap: 25px; padding: 5px; width: 100%;",
      uiOutput(ns("groups_vertical_stack"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    group_counter <- reactiveVal(1)
    active_groups <- reactiveVal(1)

    # 1. We can write to reactiveValues at startup...
    group_names_map <- reactiveValues()
    group_names_map[["1"]] <- "Primary Visualization Group"

    current_view <- reactiveVal("all")

    # 2. FIX: Pass the raw string directly here instead of reading from group_names_map[["1"]]
    group_server(
      id = "group_instance_1",
      assigned_name = "Primary Visualization Group",
      remove_group_callback = function() {
        active_groups(setdiff(active_groups(), 1))
        if (current_view() == "1") current_view("all")
      }
    )

    # Step 1: Open the modal to request Group configurations
    observeEvent(input$trigger_group_modal, {
      next_num <- group_counter() + 1
      showModal(modalDialog(
        title = "📁 Setup New Group Environment",
        textInput(
          ns("modal_group_name"),
          "Assign Group Name",
          value = paste("Analytics Group", next_num)
        ),
        footer = list(
          actionButton(
            ns("submit_new_group"),
            "Initialize Group",
            class = "btn-primary"
          ),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
    })

    # Step 2: Validate details, spin up module, and switch view focus
    observeEvent(input$submit_new_group, {
      req(input$modal_group_name)
      removeModal()

      new_id <- group_counter() + 1
      group_counter(new_id)

      id_str <- as.character(new_id)
      chosen_name <- input$modal_group_name

      # Writing here is perfectly fine because we are inside a reactive consumer (observeEvent)
      group_names_map[[id_str]] <- chosen_name

      local({
        current_g_id <- new_id
        g_id_str <- id_str
        group_server(
          id = paste0("group_instance_", g_id_str),
          assigned_name = chosen_name,
          remove_group_callback = function() {
            active_groups(setdiff(active_groups(), current_g_id))
            if (current_view() == g_id_str) current_view("all")
          }
        )
      })

      active_groups(c(active_groups(), new_id))
      current_view(id_str)
    })

    # Step 3: Render the Sidebar Dropdown Directory
    output$sidebar_navigation <- renderUI({
      g_ids <- active_groups()
      choices_list <- c("Show All Groups" = "all")

      # Reading here is perfectly fine because we are inside a reactive consumer (renderUI)
      if (length(g_ids) > 0) {
        for (g_id in g_ids) {
          id_str <- as.character(g_id)
          display_label <- group_names_map[[id_str]]
          choices_list[display_label] <- id_str
        }
      }

      shiny::selectInput(
        ns("selected_group_view"),
        label = "Navigate Groups:",
        choices = choices_list,
        selected = current_view()
      )
    })

    # Sync our internal reactive tracker whenever the user interacts with the dropdown
    observeEvent(input$selected_group_view, {
      current_view(input$selected_group_view)
    })

    # Step 4: Render groups conditionally based on the active navigation filter
    output$groups_vertical_stack <- renderUI({
      g_ids <- active_groups()

      if (length(g_ids) == 0) {
        return(p(
          "No dynamic groups generated yet. Click 'Add New Group' to initialize layouts.",
          style = "color: #7f8c8d; font-style: italic; text-align: center; margin-top: 50px;"
        ))
      }

      visible_ids <- if (current_view() == "all") {
        g_ids
      } else {
        intersect(g_ids, as.numeric(current_view()))
      }

      if (length(visible_ids) == 0) {
        return(p(
          "The selected group is no longer active.",
          style = "color: #7f8c8d; font-style: italic;"
        ))
      }

      lapply(visible_ids, function(g_id) {
        group_ui(ns(paste0("group_instance_", g_id)))
      })
    })
  })
}
