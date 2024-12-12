Error: Data too long for column 'PM10' at row 1 [1406] 
Traceback: 
 [1] "16: result_bind(res@ptr, params)"                                   
 [2] "15: dbBind(rs, params)"                                             
 [3] "14: dbBind(rs, params)"                                             
 [4] "13: dbSend(conn, statement, params, is_statement = TRUE, immediate)"
 [5] "12: .local(conn, statement, ...)"                                   
 [6] "11: dbSendStatement(conn, statement, ...)"                          
 [7] "10: dbSendStatement(conn, statement, ...)"                          
 [8] "9: dbExecute(conn, query, params = unname(as.list(value)))"         
 [9] "8: dbExecute(conn, query, params = unname(as.list(value)))"         
[10] "7: .nextMethod(conn = conn, name = name, value = value)"            
[11] "6: callNextMethod()"                                                
[12] "5: dbAppendTable(conn = conn, name = name, value = value)"          
[13] "4: dbAppendTable(conn = conn, name = name, value = value)"          
[14] "3: .local(conn, name, value, ...)"                                  
[15] "2: dbWriteTable(conw, \"Hcab\", nd_Hcab, append = T)"               
[16] "1: dbWriteTable(conw, \"Hcab\", nd_Hcab, append = T)"               

