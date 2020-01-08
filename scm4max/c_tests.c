// c file for testing out my C code easily
#include "stdio.h"
#include "stdint.h"
#include "string.h"
#include "stdbool.h"
#include "stdlib.h"

// return true if a string begins and ends with quotes
int is_quoted(char *string){
    return string[0] == '"' && string[ strlen(string)-1 ] == '"';
}  
char *trim_quotes(char *input){
    int length = strlen(input);
    char *trimmed = malloc( sizeof(char*) * length );
    for(int i=0, j=0; i<length; i++){
        if( input[i] != '"'){
            trimmed[j] = input[i];
            j++;            
        }    
    }
    return trimmed;
}   

int main(){
    printf("Running\n");

    char *no_quotes = "foobar";
    char *quotes = "\"foobar\"";

    printf("checking %s : %d\n", no_quotes, is_quoted(no_quotes));
    printf("checking %s : %d\n", quotes, is_quoted(quotes));
   
    printf("trim_quotes %s : %s\n", quotes, trim_quotes(quotes) ); 
}
