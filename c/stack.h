#ifndef STACK_H 
#define STACK_H 

struct Node* createStack(int data);

int pop_stack(struct Node** stack);

void push_stack(int data, struct Node** stack);

int peek_stack(struct Node* stack);

#endif