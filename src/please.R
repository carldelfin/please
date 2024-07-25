#!/usr/bin/env Rscript

# --------------------------------------------------------------------------------------------------
# Please tokens
# --------------------------------------------------------------------------------------------------

please_tokens <- c(
    LETTER = '[[:alpha:]]',
    NUMBER = '[0-9]+',
    SYMBOL = '[[:punct:]]',
    FUNC = 'say',
    FUNC = 'scream',
    FUNC = 'add',
    FUNC = 'multiply',
    QUOTE = '"',
    WHITESPACE = '\\s+',
    EOI = 'please',
    CLEAR = 'clear',
    EXIT = 'exit')

# --------------------------------------------------------------------------------------------------
# Please lexer
# --------------------------------------------------------------------------------------------------

please_lexer <- function(input) {

    matches <- vector("character", length = nchar(input))
    
    for (i in 1:length(please_tokens)) {
        
        token <- please_tokens[i]
        token_name <- names(token)
        pos <- gregexpr(token, input)[[1]]
        
        if (pos[[1]] != -1) {
    
            tmp <- data.frame(pos_start = pos[pos > 0],
                              pos_len = attr(pos, "match.length"))
            
            for (row in 1:nrow(tmp)) {
                
                start <- tmp[row, "pos_start"]
                end <- start + tmp[row, "pos_len"] - 1
                matches[start:end] <- token_name
                
            }
            
        }
        
    }
    
    lex_output <- data.frame(character = strsplit(input, "")[[1]], token = matches)
    return(lex_output)
    
}

please_parser <- function(input) {
    
    # RULE 1:
    # Exit program if the first four characters in input are 'exit',
    # clear output if the first five characters in input are 'clear'
    if (length(grep("EXIT", input[1:4, "token"])) == 4) {
        expr <- ("EXIT")
    } else if (length(grep("CLEAR", input[1:5, "token"])) == 5) {
        expr <- ("CLEAR")
    } else {
            
        input_len <- nrow(input)
    
        # ------------------------------------------------------------------------------------------
        # General syntax rules for function calls
        # ------------------------------------------------------------------------------------------
        
        # RULE 2:
        # Input must begin with a valid function call
        if (input[1, "token"] != "FUNC") {
            stop("Syntax error: Input must begin with a valid function call")
        } else {
            
            # get function name, and _only_ the first instance in case there are several
            lag_token <- c(NA, input$token[-nrow(input)])
            lag_token[1] <- input$token[1]
            cumsum_condition <- cumsum(input$token != lag_token)
            input_func <- input[cumsum_condition < 1, ]
            func_name <- paste0(input_func$character[input_func$token == "FUNC"], collapse = "")
            
            # RULE 3:
            # First character after function call must be a whitespace
            if (input[nchar(func_name) + 1, "token"] != "WHITESPACE") {
                stop("Syntax error: Missing whitespace after function call")
            }
            
        }
        
        # ------------------------------------------------------------------------------------------
        # Syntax rules for the 'please' keyword
        # ------------------------------------------------------------------------------------------
        
        # RULE 4:
        # Every input must end with the 'please' keyword
        if (length(grep("EOI", input[as.integer(nrow(input) - nchar("please") + 1):nrow(input), "token"])) != 6) {
            stop("Syntax error: Keyword 'please' is not at end of input")
        } 
            
        # RULE 5:
        # There must be a whitespace before the 'please' keyword
        if (input[nrow(input) - nchar("please"), "token"] != "WHITESPACE") {
            stop("Syntax error: Missing whitespace before 'please' keyword")
        }
        
        # ------------------------------------------------------------------------------------------
        # Syntax rules for the 'say' and 'scream' functions
        # ------------------------------------------------------------------------------------------
        
        if (func_name == "say" | func_name == "scream") {
            
            # RULE 6:
            # The second character after a call to 'say' or 'scream' must be a double quote
            if (input[nchar(func_name) + 2, "token"] != "QUOTE") {
                stop(paste0("Syntax error: Missing double quote after call to '", func_name, "'"))
            }
            
            # RULE 7:
            # There must be another double quote following the first one
            if (length(grep("QUOTE", input[as.integer(nchar(func_name) + 3):input_len, "token"])) == 0) {
                stop(paste0("Syntax error: Strings following call to '", func_name, "' must be enclosed in double quotes"))
            }
            
            # RULE 8:
            # There can only be two double quotes
            if (length(grep("QUOTE", input[, "token"])) > 2) {
                stop("Syntax error: Double quotes may only be used to enclose strings")
            }

            # treat everything between double quotes as string to output
            str_to_output <- sub('[^\"]+\"([^\"]+).*', '\\1', paste0(input[, "character"], collapse = ""))
            
            if (func_name == "scream") {
                str_to_output <- toupper(str_to_output)
            } else {
                str_to_output <- tolower(str_to_output)
            }
            
            expr <- paste0("print(", deparse(str_to_output), ")")
            
        }
        
        # ------------------------------------------------------------------------------------------
        # Syntax rules for the 'add' and 'multiply' functions
        # ------------------------------------------------------------------------------------------
        
        if (func_name == "add" | func_name == "multiply") {
            
            # RULE 9:
            # The second character after a call too 'add'/'multiply' to must be a number
            if (input[nchar(func_name) + 2, "token"] != "NUMBER") {
                stop(paste0("Syntax error: Missing number after call to '", func_name, "'"))
            }
            
            # RULE 10:
            # There must be at least one more number
            if (length(grep("NUMBER", input[as.integer(nchar(func_name) + 3):input_len, "token"])) < 1) {
                stop(paste0("Syntax error: At least two numbers are required for '", func_name, "'"))
            }
            
            # RULE 11:
            # All numbers must be followed by another number or a whitespace
            num_vec <- input[as.integer(nchar(func_name) + 1):input_len, "token"]
            for (i in seq_along(num_vec)) {
                if (num_vec[i] == "NUMBER" && (i == length(num_vec) || !num_vec[i + 1] %in% c("WHITESPACE", "NUMBER"))) {
                    stop("Syntax error: Numbers must be followed by another number or a whitespace")
                }
            }
            
            # get all numbers to add or multiply; this is a bit tricky since numbers can come in groups,
            # such as '123' or '99', and this needs to be taken into consideration
            num_to_add <- as.numeric(input[grep("NUMBER", input[, "token"]), "character"])
            grouped_numbers <- list()
            
            current_group <- NULL
            previous_token <- NULL
            
            for (i in 1:nrow(input)) {
                if (input$token[i] == "NUMBER") {
                    if (is.null(current_group) || previous_token != "NUMBER") {
                        current_group <- input$character[i]
                    } else {
                        current_group <- paste0(current_group, input$character[i])
                    }
                } else {
                    if (!is.null(current_group)) {
                        grouped_numbers <- c(grouped_numbers, as.numeric(current_group))
                        current_group <- NULL
                    }
                }
                previous_token <- input$token[i]
            }
            
            if (!is.null(current_group)) {
                grouped_numbers <- c(grouped_numbers, as.numeric(current_group))
            }
            
            grouped_numbers <- unlist(grouped_numbers)

            if (func_name == "add") {
                expr <- paste0("print(sum(", deparse(grouped_numbers), "))")
            } else {
                expr <- paste0("print(prod(", deparse(grouped_numbers), "))")
            }
            
        }
        
    }
   
    # ----------------------------------------------------------------------------------------------
    # Return expression
    # ----------------------------------------------------------------------------------------------
    
    return(expr)
    
}

initiate_repl <- function() {
    
    system("clear")
    message("This is please version 1.0.0\n",
            "See documentation for details:\n",
            "https://github.com/carldelfin/please\n")
    
    while (TRUE) {
        
        cat("~ ")
        please_prompt <- readLines("stdin", n = 1)

        pars_output <- tryCatch(
            please_parser(please_lexer(please_prompt)),
            error = function(cond) {
                message(conditionMessage(cond))    
                NA
            }
        )

        if (!is.na(pars_output) & pars_output == "EXIT") {
            quit(save = "no")
        } else if (!is.na(pars_output) & pars_output == "CLEAR") {
            system("clear")
        } else if (!is.na(pars_output)) {
            eval(parse(text = pars_output))
        }
        
    }
}

initiate_repl()