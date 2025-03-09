#include "s4m-helpers.h"

/********************************************************************************
/ some helpers */
// return true if a string begins and ends with quotes
int in_quotes(char *string){
    //post("in_quotes, input: %s", string);
    if(string[0] == '"' && string[ strlen(string)-1 ] == '"'){
        return 1;
    }else{
        return 0;
    }
} 
char *trim_quotes(char *input){
    int length = strlen(input);
    char *trimmed = malloc( sizeof(char*) * length );
    for(int i=0, j=0; i<length; i++){
        if( input[i] != '"'){ trimmed[j] = input[i]; j++; }    
    }
    return trimmed;
}   
// return true if a string starts with a single quote
int is_quoted_symbol(char *string){
    if(string[0] == '\'' && string[ strlen(string)-1 ] != '\''){
        return 1;
    }else{
        return 0;
    }
}
char *trim_symbol_quote(char *input){
    // drop the first character (the quote)
    char *trimmed = malloc( sizeof(char*) * (strlen(input) - 1) );
    int i;
    for(i=1; input[i] != '\0'; i++){
        trimmed[i-1] = input[i]; 
    }
    trimmed[i-1] = '\0';
    return trimmed;
}

