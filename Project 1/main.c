#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct stackNode {
    struct stackNode *next;
    int number;
};

struct stackNode *push(struct stackNode *head, int number) {
    struct stackNode *temp = (struct stackNode *) malloc(sizeof(struct stackNode));
    if (temp == NULL) { // If malloc fails
        exit(1);
    }
    temp->number = number;
    temp->next = head;
    head = temp;
    return head;
}

struct stackNode *pop(struct stackNode *head, int *number) {
    struct stackNode *temp = head;
    *number = head->number;
    head = head->next;
    free(temp);
    return head;
}

int main() {
    FILE *file;
    char line[9];
    int numLines;
    size_t length = 0;

    // Open file and make sure it worked
    file = fopen("../input.txt", "r");
    if (file == NULL){
        printf("Error! opening file");
        exit(1);
    }

    // Read first line to determine how many more lines there are
    fgets(line, sizeof(line), file);
    numLines = atoi(line);

    char instructions[numLines][5];
    int numbers[numLines];

    // For each line read, allocate memory based on length of string and copy the string into the array
    for(int i = 0; i < numLines; i++) {
        fgets(line, sizeof(line), file);
        // Remove trailing newline when applicable
        if(line[strlen(line) - 1] == '\n') {
            line[strlen(line) - 1] = '\0';
        }
        sscanf(line, "%s %d", instructions[i], &numbers[i]);
    }

    // Close file
    fclose(file);

    char input[10];

    struct stackNode *head = NULL;

    for(int i = 0; i < numLines; i++) {
        if(strcmp(instructions[i], "IN") == 0) {
            printf("Please enter an integer: ");
            fgets(input, sizeof(input), stdin);
            head = push(head, atoi(input));
        } else if(strcmp(instructions[i], "OUT") == 0) {
            int popped;
            head = pop(head, &popped);
            printf("Popped %d\n", popped);
        } else if(strcmp(instructions[i], "LIT") == 0) {
            head = push(head, numbers[i]);
        } else if(strcmp(instructions[i], "DROP") == 0) {
            int temp;
            head = pop(head, &temp);
        } else if(strcmp(instructions[i], "DUP") == 0) {
            int duplicates[numbers[i]];
            for(int j = 0; j < numbers[i]; j++) {
                head = pop(head, &duplicates[j]);
            }
            for(int j = 0; j < 2; j++) {
                for(int k = numbers[i] - 1; k >= 0; k--) {
                    head = push(head, duplicates[k]);
                }
            }
        } else if(strcmp(instructions[i], "SWAP") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            head = push(head, a);
            head = push(head, b);
        } else if(strcmp(instructions[i], "ADD") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            int sum = a + b;
            head = push(head, sum);
        } else if(strcmp(instructions[i], "SUB") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            int difference = a - b;
            head = push(head, difference);
        } else if(strcmp(instructions[i], "MUL") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            int product = a * b;
            head = push(head, product);
        } else if(strcmp(instructions[i], "DIV") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            int quotient = a / b;
            head = push(head, quotient);
        } else if(strcmp(instructions[i], "MOD") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            int remainder = a % b;
            head = push(head, remainder);
        } else if(strcmp(instructions[i], "AND") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            int and = a & b;
            head = push(head, and);
        } else if(strcmp(instructions[i], "OR") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            int or = a | b;
            head = push(head, or);
        } else if(strcmp(instructions[i], "IFEQ") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            if(a == b) {
                int jump = numbers[i] - 1;
                i = --jump;
            }
        } else if(strcmp(instructions[i], "IFLT") == 0) {
            int a, b;
            head = pop(head, &a);
            head = pop(head, &b);
            if(a < b) {
                int jump = numbers[i] - 1;
                i = --jump;
            }
        } else if(strcmp(instructions[i], "JUMP") == 0) {
            int jump = numbers[i] - 1;
            i = --jump;
        }
    }
    return 0;
}
