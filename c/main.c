#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include "stack.h"
#include "tags.h"

struct Node* stack;

// the length of the biggest tag name + 1 for null terminator
// in this case is the title(5) + null terminator(1) = 6
const int maxTagLength = 6;

// a value that is not assigned to tagID, this will be added to the stack
// and pop at EOF. If the popped value is not equal to this value it means 
// there was a mismatch
const int specialStartingValueForStack = 0;

bool hasHTMLTag = false;
bool hasBodyTag = false;
bool hasTitleTag = false;
bool hasHeadTag = false;
bool hasBodytag = false;


int convertTagNameToInt(char *tag){
    if (!strcmp(tag, HTML_TAG)){
        return HTML_TAG_CODE;
    }

    if(!strcmp(tag, HEAD_TAG)){
        return HEAD_TAG_CODE;
    }

    if(!strcmp(tag, BODY_TAG)){
        return BODY_TAG_CODE;
    }

    if(!strcmp(tag, TITLE_TAG)){
        return TITLE_TAG_CODE;
    }

    if(!strcmp(tag, H1_TAG)){
        return H1_TAG_CODE;
    }

    if(!strcmp(tag, H2_TAG)){
        return H2_TAG_CODE;
    }

    if(!strcmp(tag, H3_TAG)){
        return H3_TAG_CODE;
    }

    if(!strcmp(tag,P_TAG)){
        return P_TAG_CODE;
    }

    if(!strcmp(tag, UL_TAG)){
        return UL_TAG_CODE;
    }

    if(!strcmp(tag, LI_TAG)){
        return LI_TAG_CODE;
    }

    if(!strcmp(tag, A_TAG)){
        return A_TAG_CODE;
    }

    if(!strcmp(tag, DIV_TAG)){
        return DIV_TAG_CODE;
    }

    if(!strcmp(tag, BR_TAG)){
        return BR_TAG_CODE;
    }

    if(!strcmp(tag, HR_TAG)){
        return HR_TAG_CODE;
    }

    return -1;
}

void validateNestingTag(int tagID, int top){

    if (tagID != HTML_TAG_CODE && top == specialStartingValueForStack){
        printf("all tags need to be inside <html> tag except <html> tag itself");
        exit(-1);
    }

    if (tagID == HEAD_TAG_CODE && top != HTML_TAG_CODE){
        printf("head tag should be children html tag");
        exit(-1);
    }

    if (tagID == TITLE_TAG_CODE && top != HEAD_TAG_CODE){
        printf("title tag needs to be inside of head tag");
        exit(-1);
    }

    if (tagID == DIV_TAG_CODE && top == P_TAG_CODE){
        printf("Div incorrectly nested inside p tag");
        exit(-1);
    }

    if (tagID == P_TAG_CODE && top == P_TAG_CODE){
        printf("p tag cannot be nested in another p tag");
        exit(-1);
    }

    if (tagID != TITLE_TAG_CODE && top == HEAD_TAG_CODE){
        printf("only title tag can be children of head tag");
        exit(-1);
    }

    if (tagID != HEAD_TAG_CODE && tagID != BODY_TAG_CODE && top == HTML_TAG_CODE){
        printf("tags other than <head> and <body> should be children of either head or body, not children of <html>");
        exit(-1);
    }

    if (tagID == HTML_TAG_CODE){
        if (hasHTMLTag == false){
            hasHTMLTag = true;
        }else {
            printf("duplicate html tag");
            exit(-1);
        }
    }
}

void validateStructure(int tagID, int top){
    if (tagID == BODY_TAG_CODE){
        if (hasBodyTag == false){
            hasBodyTag = true;
        }else {
            printf("duplicate body tags");
            exit(-1);
        }
    }

    if (tagID == TITLE_TAG_CODE){
        if (hasTitleTag == false){
            hasTitleTag = true;
        }else {
            printf("duplicate title tags");
            exit(-1);
        }
    }

    if (tagID == HEAD_TAG_CODE && hasBodyTag){
        printf("head tag cannot come after body tag");
        exit(-1);
    }
}

void validatePushAction(int tagID){

    int top = peek_stack(stack);

    validateNestingTag(tagID, top);
    validateStructure(tagID, top);
}

bool validateRequiredTags(){
    return hasHTMLTag && hasBodyTag;
}

bool handleStackActionForTags(int tag_id, bool add){
    if (tag_id == -1){
        printf("invalid tag name");
        exit(-1);
    }

    if (add){
        validatePushAction(tag_id);

        if (tag_id == HR_TAG_CODE || tag_id == BR_TAG_CODE){
            return true;
        }
        push_stack(tag_id, &stack);

        return true;
    }else {
        int poppedStack = pop_stack(&stack);

        if (tag_id == poppedStack){
            return true;
        }else {
            printf("mismatched tags");
            exit(-1);
            return false;
        }
    }
    return false;
}

void checkInvalidUseOfAngle(char c){
    if (c == '<' || c == '>'){
        printf("invalid use of angle brackets");
        exit(-1);
    }
}

void appendCharToTagName(char *tagName, char c){
            // adding one char each time so initially both char can be the null terminator
            // and index into the first char to swap with our actual character
            char currentCharacter[2] = "\0";
            currentCharacter[0] = c;
            strncat(tagName, currentCharacter, 1);
}

void readTillEndTag(FILE *file){
    // we use 7 here since the longest tag is /title + null terminator which is 7
    char tagName[maxTagLength] = "\0";
    int tagLength = 0;
    bool metSpace = false;
    bool first = true;
    bool closingTag = false;
    
    while (true){
        char c = fgetc(file);

        if (first && c == '/' && metSpace == false){
            closingTag = true;
            first = false;
            continue;
        }

        if (c == '>' || c == '<'){
            break;
        }

        if (c == ' '){
            // we don't care about the attribute after the tag's name so ignore everything when
            // space is found until the char '>'
            metSpace = true;
            continue;
        }

        if (!metSpace && (c != ' ' || c != '\n')){
            tagLength += 1;

            if (tagLength == maxTagLength) {
                printf("invalid tag");
                exit(-1);
            }

            appendCharToTagName(tagName, tolower(c));
        }
    }
    handleStackActionForTags(convertTagNameToInt(tagName), !closingTag);
}

void readFile(char *filePath){
    FILE *file = fopen(filePath, "r");

    if (file == NULL){
        printf("File does not exist");
        return;
    }

    while (true){
        char c = fgetc(file);
        
        if (c == '<'){
            readTillEndTag(file);
            continue;
        }

        if (c == EOF){

            if(pop_stack(&stack) != specialStartingValueForStack){
                printf("mismatched tags");
                exit(-1);
            }

            if(!validateRequiredTags()){
                printf("requires atleast html and body tag");
                exit(-1);
            }

            printf("html is valid\n");
            break;
        }
    }
}

int main(){
    stack = createStack(specialStartingValueForStack);
    readFile("file.html");
}