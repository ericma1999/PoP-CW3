#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct Node {
    int data;
    struct Node* nextNode;
};


int peek_stack(struct Node* stack){
    if (stack == NULL){
        return -1;
    }

    return stack->data;
}

void push_stack(int data, struct Node** stack){
    struct Node* newNode = malloc(sizeof(struct Node));

    newNode->data = data;
    newNode->nextNode = *stack;

    // single dereference here to point to the rootNode variable, not the memory of the value of the rootnode
    // which would change our nextNode that was assigned
    *stack = newNode;
}

int pop_stack(struct Node** stack){
    if (*stack == NULL){
        return -1;
    }

    struct Node* currentTopNode = *stack;
    
    int result = currentTopNode->data;

    *stack = (*stack)->nextNode;

    free(currentTopNode);

    return result;
}

struct Node* createStack(int data){
    struct Node* rootNode = malloc(sizeof(struct Node));

    rootNode->data = data;

    return rootNode; 
}


// int main(){
    // struct Node* rootNode = malloc(sizeof(struct Node));

    // rootNode->data = 5;

    // struct Node* rootNode = createStack(0);

    // push_stack(10, &rootNode);

    // push_stack(15, &rootNode);

    // printf("%i\n", peek_stack(rootNode));

//     printf("%i\n", pop_stack(&rootNode));

//     printf("%i\n", pop_stack(&rootNode));
// }