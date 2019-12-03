tokenize = function(str) {
  str = gsub(pattern="\\s+",x=str,replacement = "",perl=TRUE)
  pos=gregexpr(str,pattern = "([\\(\\)\\+\\*\\-\\/\\^\\,])|(\\%\\%)",perl=TRUE)[[1]]
  pos2 = as.data.frame(cbind(pos=pos,length=attr(pos,"match.length")))
  
  tokens = c()
  
  for (i in 1:nrow(pos2)) {
    row = pos2[i,]
    
    if (i < nrow(pos2)) {
      start1 = row[,1]
      stop1 = row[,1]+row[,2]-1
      start2 = row[,1]+row[,2]
      stop2 = pos2[i+1,1]-1
      
      if (i==1 && start1 > 1) {
        tokens = c(trimws(substr(str,start=1,stop = start1-1)),trimws(substr(str,start=start1,stop = stop1)), trimws(substr(str,start=start2,stop = stop2)))
        next
      }
      
      if (start2<=stop2) {
        tokens = c(tokens,c(trimws(substr(str,start=start1,stop = stop1)), trimws(substr(str,start=start2,stop = stop2))))
      } else {
        tokens = c(tokens,trimws(c(substr(str,start=start1,stop = stop1))))
      }
      
    } else {
      tokens = c(tokens,trimws(substr(str,start=row[,1],stop = row[,1]+row[,2]-1)))
      
      if (row[,1]+row[,2]-1 < nchar(str)) {
        tokens = c(tokens,trimws(substr(str,start=row[,1]+row[,2], stop = nchar(str))))
      }
    }
    
    for (i in 1:length(tokens)) {
      if (nchar(tokens[i]) == 0) {
        tokens[i] = NA
      }
    }
    if (sum(is.na(tokens))>0) {
      tokens = tokens[-which(is.na(tokens))]
    }
    
    
    for(i in 1:length(tokens)) {
      if (i > 1) {
        last_closed = endsWith(tokens[i-1],suffix = ",") || endsWith(tokens[i-1],suffix = "(")
      }  else {
        last_closed = FALSE
      }
      
      if (i < length(tokens)) {
        next_number = grepl("^[[:digit:]]+L?",tokens[i+1])
      } else {
        next_number = FALSE
      }
      
      if (tokens[i] == "-" && next_number && last_closed) {
        tokens[i] = NA
        tokens[i+1] = paste0("-",tokens[i+1])
        i = i+2
      }
      
      
    }
    if (sum(is.na(tokens))>0) {
      tokens = tokens[-which(is.na(tokens))]
    }
    
  }
  
  
  return(tokens)
}


# based on the pseudo code at https://de.wikipedia.org/wiki/Shunting-yard-Algorithmus (German) the english version
# lacked support for argument separators
#
# Translation of pseudo code:
#
# init stack
# init output
# WHILE tokens are available:
#   read token.
#   IF token is a number:
#     add token to output.
#   ENDIF
#   IF token is a function:
#     push token to stack.
#   ENDIF
#   IF token is separator:
#     WHILE peek stack is not '(':
#       pop stack and add to output.
#       ERROR-IF stack is empty:
#         REASON (1) Misplaced separator.
#         REASON (2) No '(' for found closing paranthesis.
#       ENDERROR
#     ENDEBIS
#   ENDIF
#   IF token is operator
#     WHILE stack is not empty AND
#             peek stack is operator AND
#             token is left associative AND
#             precedence of token <= precedence of peek stack
#       pop stac and add to output.
#     ENDWHILE
#     push token to stack.
#   ENDIF
#   IF token = '(':
#     push token to stack.
#   ENDIF
#   IF token = ')':
#     WHILE peek stack != '(':
#       ERROR-IF Stack is empty:
#         REASON (1) No matching '(' found for closing paranthesis.
#       ENDERROR
#       pop stack and add to output.
#     ENDEBIS
#     pop stack (it should be '(')
#     IF peek stack is function:
#       pop stack and add to output.
#     ENDIF
#   ENDIF
# ENDWHILE
# WHILE Stack is not empty:
#   ERROR-IF peek stack == '(':
#     REASON (1) More opening paranthesis '(' found as closing ones.
#   ENDERROR
#   pop stack and add to output.
# ENDWHILE

math_parse = function(formular) {
  tokens = tokenize(formular)
  stack = c()
  output = c()
  
  precedence = as.data.frame(cbind(operator=c("^","*","/","+","-","%%"),
                                   precedence=c(4,3,3,2,2,4),
                                   associativity = c("r","l","l","l","l","l")),
                             stringsAsFactors = FALSE)
  
  pfun = as.data.frame(cbind(operator = c("sin","cos","tan","asin","acos","atan",
                                          "abs","min","max","mean","median","sum",
                                          "sd","var","sqrt","sign","trunc"),
                             precedence = 1,
                             associativity = "l"),stringsAsFactors = FALSE)
  
  precedence = rbind(precedence,pfun)
  
  for (c in 1:length(tokens)) {
    token = tokens[c]
    
    token_is_number = grepl("^-?[[:digit:]]+L?",token)
    token_is_array_elem = grepl("\\[\\d+\\]",token)
    if (token_is_number || token_is_array_elem) {
      output = c(output,token)
    }
    
    token_is_fun = token %in% precedence$operator && 
      precedence[precedence$operator == token,"precedence"] == 1 
    if (token_is_fun) {
      stack = c(token,stack)
    }
    
    token_is_separator = token == ","
    if (token_is_separator) {
      while(stack[1] != "(") {
        output = c(output,stack[1])
        stack = stack[-1]
      }
      output = c(output,token)
    }
    
    token_is_operator = token %in% precedence$operator && 
      precedence[precedence$operator == token,"precedence"] > 1
    if(token_is_operator) {
      while(length(stack) > 0 &&
            (stack[1] %in% precedence$operator && 
             precedence[precedence$operator == stack[1],"precedence"] > 1) &&
            (token %in% precedence$operator &&
             precedence[precedence$operator == token,"associativity"] == "l") && 
            (token %in% precedence$operator &&
             precedence[precedence$operator == token,"precedence"] <= precedence[precedence$operator == stack[1],"precedence"])) {
        output = c(output,stack[1])
        stack = stack[-1]
      }
      stack = c(token, stack)
    }
    
    token_is_open_bracket = token == "("
    if (token_is_open_bracket) {
      stack = c(token,stack)
    }
    
    token_is_close_bracket = token == ")"
    if (token_is_close_bracket) {
      while(length(stack) > 0 && stack[1] != "(") {
        if (length(stack) == 1 && stack[1] != "(") stop("Closing bracket has no prior opening bracket")
        
        output = c(output,stack[1])
        stack = stack[-1]
      }
      if (stack[1] == "(") {
        stack = stack[-1]
      }
      
      stack_top_is_fun = stack[1] %in% precedence$operator && 
        precedence[precedence$operator == stack[1],"precedence"] == 1
      if (stack_top_is_fun) {
        output = c(output,stack[1])
        stack = stack[-1]
      }
    }
  }
  
  if (length(stack) > 0) {
    while(length(stack) > 0) {
      if (stack[1] == "(") stop("There are more opening brackets, then closing ones")
      
      output = c(output,stack[1])
      stack = stack[-1]
    }
  }
  
  return(output)
}

call_tree = function(postfix_ast) {
  # compute_rpn(input)
  #   stack_init
  #   foreach (o in input)
  #     switch o
  #       isnumber
  #         push o
  #       isfunction
  #         unary_arg = pop
  #         if peek is ',' operand
  #           while peek is ',' operand
  #             pop and add to temporary stack
  #           args = join unary_arg and temporary stack
  #           push list(o,args)
  #         else
  #           push list(o, unary_arg)
  #       isseparator
  #         unary_arg = pop
  #         push list(o, unary_arg)
  #       isbinoperator
  #         right = pop
  #         left = pop
  #         push list(o,left, right)
  #   return pop
  
  ops = c("+","/","-","*","^")
  
  fun = c("sin","cos","tan","asin","acos","atan","abs","min","max","mean","median","sum","sd","var")
  
  operand_stack = list()
  for (i in 1:length(postfix_ast)) {
    token = postfix_ast[i]
    if (token %in% fun) { # function
      # treat functions as unary
      unary_arg = operand_stack[1]
      operand_stack = operand_stack[-1]
      
      # check if the next is a separator 
      if (operand_stack[[1]]$operator == ",") { #n-ary
        # add this and the following separator arguments until the next is not one of thoses
        args = list(unary_arg)
        while (operand_stack[[1]]$operator == ",") {
          arg = operand_stack[[1]]
          arg$operator = NULL
          
          operand_stack = operand_stack[-1]
          args = append(arg,args)
        }
        
        operand = append(list(operator=token),lapply(args,function(elem)elem[[1]]))
        operand_stack = append(list(operand),operand_stack)
      } else { # unary
        operand = list(operator=token,unary_arg)
        operand_stack = append(list(operand),operand_stack)
      }
      
    } else if (token %in% ops) { # operation
      
      right = operand_stack[[1]]
      operand_stack = operand_stack[-1]
      
      
      left = operand_stack[[1]]
      operand_stack = operand_stack[-1]
      
      operand = list(operator=token,left,right)
      operand_stack = append(list(operand),operand_stack)
    } else if (token == ",") { # separator
      last_operand = operand_stack[1]
      operand_stack = operand_stack[-1]
      
      operand = list(operator=token, last_operand)
      operand_stack = append(list(operand), operand_stack)
    } else { # argument
      if (grepl("^-?[[:digit:]]+L?",token)) {
        token = as.numeric(token)
      }
      
      operand_stack = append(list(token), operand_stack)
    }
    
    
  }
  return(operand_stack[[1]])
  
  # list(op, arg2, arg1) important, because / has order!
}


replace_fun = function(call_graph,process_mapping,data) {
  operation = call_graph
  if (!"operator" %in% names(operation)) {
    if (grepl(operation,pattern = "\\[",perl=TRUE)) {
      index = as.integer(sub(x = operation,pattern = ".*\\[(\\d+)\\]",replacement = "\\1",perl=TRUE))-1
      FUN = process_mapping[process_mapping$r == "[","FUN"]
      if (is.null(FUN[[1]])) {
        stop("Back-end does not offer process: 'array_element'")
      }
      return(list(operator=FUN[[1]],data,index))
    } else {
      return(operation)
    }
    
  } 
  else {
    sel = process_mapping[process_mapping$r == operation$operator,]
    if (nrow(sel) < 1) stop(paste0("No match defined for R operator / function: '",operation$operator,"'"))
    
    if (!any(!sapply(sel$FUN,is.null))) stop(paste0("Back-end does not offer neither process(es): ",paste(paste0("'",sel[,"openeo"],"'"),sep="",collapse=", ")))
    
    FUN = sel$FUN[!sapply(sel$FUN,is.null)][[1]]
    operator = operation$operator
    operation$operator = NULL
    
    args = lapply(operation,replace_fun,process_mapping,data)
    
    
    return(append(list(operator=FUN),args)) 
  }
  
}

eval_call = function(call) {
  
  FUN = call$operator
  call$operator=NULL
  for (i in 1:length(call)) {
    if (is.list(call[[i]])) {
      call[[i]] = eval_call(call[[i]])
    }
  }
  
  call = unname(call)
  arg_names = names(formals(FUN))
  
  # note: this might be problematic as we don't know if the back-end has specified it that way;
  # however the core processes should be implemented
  if ("data" %in% arg_names) {
    # all args go into this
    if (all(c("data","index","return_nodata") %in% arg_names)) { # array_element
      if (length(call) == 2) {
        names(call) = c("data","index")
      } else {
        names(call) = c("data","index","return_nodata")
      }
      
    } else {
      call = list(data=call)
    }
    
  } else if (all(c("x","y") %in% arg_names)) {
    # x and y arguments, check if length args == 2 and then assign
    if (length(call) == 2) {
      names(call) = c("x","y")
    } else {
      stop("more than x and y found")
    }
  } else if (all(c("base","p") %in% arg_names)){
    # same as xy but this is for power
    if (length(call) == 2) {
      names(call) = c("base","p")
    } else {
      stop("more than base and p found")
    }
  } else if ("x" %in% arg_names) {
    if (length(call) == 1) {
      names(call) = "x"
    } else {
      stop("more than x found")
    }
  }
  
  return(do.call(FUN,call))
}


#' Performs band arithmetic on a data set
#' 
#' The function evaluates a function with a mathematical expression and creates internally a
#' reduce function on the spectral dimension, by converting the function stated in 'formular'
#' into a subgraph for the callback.
#' 
#' @details 
#' Similar to the 'calc' function in the raster package, band arithmetic will be performed on an
#' data vector. The select bands are available via indexing in the R way (e.g. x[1] for the first
#' value). It is important that if the bands are prefiltered the order of the bands matters otherwise 
#' it is assumed that the bands are ordered as in the collection description.
#' The expression stated in the function shall be single line and should not contain
#' a return statement.
#' 
#' @note It might occur that a back-end does not comply to the process definitions of the core
#' process definition. This is when this function will not work properly. Alternatively you
#' have to manage the graph creation by your own and work with the given processes of the back-end.
#' 
#' @param graph The graph into which the spectral reduce function will go
#' @param data A process that returns a data cube
#' @param formular A function that contains a mathematical expression
#' 
#' @examples 
#' \dontrun{
#' ...
#' data = graph$load_collection(graph$data$`COPERNICUS/S2`,
#'   spatial_extent = list(
#'      west=16.1,
#'      east=16.6,
#'      north=48.6,
#'      south= 47.2
#'    ),
#'    temporal_extent = list(
#'      "2018-01-01", "2018-02-01"
#'    ),
#'   bands=list("B08","B04","B02"))
#'  
#' evi = graph %>% band_arithmetics(data=data,function(x){
#'    2.5*((x[1]-x[2])/sum(1,x[1],6*x[2],-7.5*x[3]))
#' })
#' 
#' ...
#' }
#' @export
band_arithmetics = function(graph, data, formular) {
  con = graph$getConnection()
  
  required_processes = c("array_element", "reduce","product","subtract","sum","divide") #TBC
  if (!all(required_processes %in% names(graph))) stop(paste0("Cannot perform band arithmetics. Common processes: ",paste(setdiff(required_processes,names(graph)),sep=",")," are not defined at the back-end."))
  
  spectral_reduce =  graph$reduce(data = data,dimension = "bands")
  
  
  
  suppressMessages({
    params = callback(con = con,spectral_reduce)
    
  })
  subgraph = callback(con = con,spectral_reduce,parameter = params[1])
  
  if (is.function(formular)) {
    function_arg = names(formals(formular))[1] # should be only one... the array data others are ignored
    fbody = deparse(body(formular))
    fbody = trimws(fbody[c(-1,-length(fbody))]) # remove {}
    fbody = gsub(fbody,pattern = paste0("(",function_arg,")\\["),replacement = "[",perl=TRUE)
  } else if (is.character(formular)) {
    fbody = trimws(formular)
  } else {
    stop("Formular is no function or mathematical string expression")
  }
  
  cb_input = subgraph$data[[1]] # take the first one, to my knowledge we won't have more than one
  
  # function body -> postfix notation
  postfix = math_parse(fbody)
  graph = call_tree(postfix)
  
  # maybe put this as package data
  process_mapping = as.data.frame(cbind(openeo=c("array_element","sum","subtract","divide","multiply",
                                                 "product","power","sin","cos","tan","arcsin","arccos","arctan","absolute",
                                                 "min","max","mean","median","sum","sd","variance","sqrt","sgn","mod","int"),
                                        r = c("[","+","-","/","*","*","^","sin","cos","tan","asin","acos","atan",
                                              "abs","min","max","mean","median","sum","sd","var","sqrt","sign",
                                              "%%","trunc")
  ),stringsAsFactors = FALSE)
  process_mapping$FUN = lapply(process_mapping$openeo,function(openeo_fun){subgraph[[openeo_fun]]})
  
  calls = replace_fun(graph,process_mapping,cb_input)
  final_node = eval_call(calls)
  subgraph$setFinalNode(final_node)
  
  return(spectral_reduce)
}
