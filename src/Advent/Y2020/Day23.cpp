#include <iostream>

int GAME_SIZE = 1000000;
/* int GAME_SIZE = 9; */
int HEAD = 0;

void printArrayFrom(int cups[], int start) {
    int x = start;
    std::cout << x;
    x = cups[x];
    for (int i = 0; x != start && i < 99; ++i) {
        std::cout << " " << x;
        x = cups[x];
    }
}

void printArray(int cups[]) {
    std::cout << "Cups = (" << cups[HEAD] << ") ";
    printArrayFrom(cups, cups[cups[HEAD]]);
}

void readCups(int cups[], char * inits) {
    int prev = -1;
    int head = -1;
    int size = 0;
    for (char * c = inits; *c != '\0'; ++c) {
        int x = *c - 48;
        if (prev == -1) {
            cups[x] = x;
            head = x;
        } else {
            cups[prev] = x;
            cups[x] = head;
        }
        prev = x;
        ++size;
    }
    cups[HEAD] = head;
    // Add up to 1 million
    for (int i = size + 1; i <= GAME_SIZE; ++i) {
        cups[prev] = i;
        cups[i] = head;
        prev = i;
    }
}

void gameStep(int cups[]) {
    int head = cups[HEAD];
    int target = head - 1;
    // Select the section that will be moved
    int moveStart = cups[head];          // 1 after head
    int moveEnd = cups[cups[moveStart]]; // 2 after start

    // The next "head" node will always be the one after the moved section
    cups[head] = cups[moveEnd];
    cups[HEAD] = cups[moveEnd];

    // Find the target
    while (1) {
        if (target == 0) {
            target = GAME_SIZE;
        }

        int test = moveStart;
        for (int i = 0; test != 0 && i < 3; ++i) {
            if (test == target) {
                test = 0; // exists in the removed section
                break;
            } else {
                test = cups[test];
            }
        }
        if (test == 0)
            --target;
        else
            break;
    }

    // move the items after the target
    cups[moveEnd] = cups[target];
    cups[target] = moveStart;
}


int main(int argc, char * argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: cups inits n" << std::endl;
        return 1;
    }
    int cups[GAME_SIZE + 1];
    readCups(cups, argv[1]);
    int n = atoi(argv[2]);

    std::cout << "START = ";
    printArray(cups);
    std::cout << std::endl;
    std::cout << "N = " << n << std::endl;

    for (int i = 0; i < n; ++i) {
        gameStep(cups);
    }
    std::cout << "END = ";
    printArray(cups);
    std::cout << std::endl;

    std::cout << "From 1 = ";
    printArrayFrom(cups, 1);
    std::cout << std::endl;

    std::cout << "Result = ";
    std::cout << cups[1] * cups[cups[1]] << std::endl;

    return 0;
}
