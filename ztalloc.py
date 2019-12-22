
          
# -*- coding: utf-8 -*-
import sys
from collections import deque


#read input
data=[list(map(int,list(line.split()))) for line in open(sys.argv[1])]
Q=data[0][0]	#the number of testcases in the given file

for line in range(1,Q+1):	#repeat Q times
    lin,rin,lout,rout=data[line]
    if lin>=lout and rin<=rout:		#no need to to anything
            res='EMPTY'
            
    else:
        #easy case: the input is one known number
        if lin==rin:
            #this array is used during the bfs: 
            #seen[i]==None  -> i has not been encountred yet
            #seen[i]==0     -> i has been encountred and the previous number is (i-1)//3
            #seen[i]==1     -> i has been encountred and the previous number is 2*i
            #seen[i]==2     -> i has been encountred and the previous number is 2*i+1
            #so rather than storing the previous number, I store 1 short int. This helps reduce space in java (not sure if it works in python)
            seen=[None]*1000001       
            queue=deque([])     #queue used for bfs
            
            queue.append(lin)   #place lin=rin in queue
            seen[lin]=-1
            
            #do the bfs: 
            #if the queue becomes empty this means that all possible numbers that can be constructed with the given operations have been checked
            #and none are inside the given range
            not_found=True
            while queue and not_found:  #either find the number or check all the states
                current=queue.popleft()
                triple=3*current+1
                half=current//2
                
                if seen[half]==None:
                    seen[half]=current%2+1  #current%2 must be known when I create the output string 
                    queue.append(half)
                    if half>=lout and half<=rout:   #found the result
                        res="h"
                        not_found=False
                
                if triple<1000000 and seen[triple]==None:
                    seen[triple]=0
                    queue.append(triple)
                    if triple>=lout and triple<=rout and not_found: #found the result
                        res="t"
                        not_found=False
                
            if not_found:
                res="IMPOSSIBLE"
            
            else:
                while current!=lin:     #construct the output string using the codes in seen
                    key=seen[current]
                    if key==0:
                        res="t"+res
                        current=(current-1)//3
                        
                    else:
                        res="h"+res
                        current=2*current+key-1
            
        
        else:#lin!=rin
            #now i do exactly the same thing but with pairs where the first number is the smallest
            #now the number of bfs states is far greater and the problem is the memory
            queue=deque([])
            seen={}     #A dictionary is used now rather than a list like previously because the keys are now tuples
            queue.append((lin,rin))
            seen[(lin,rin)]=0
            
            not_found=True
            while queue and not_found:
                
                current1,current2=queue.popleft()
                triple1,triple2=3*current1+1,3*current2+1
                half1,half2=current1//2,current2//2
                
                if (half1,half2) not in seen:
                    seen[(half1,half2)]=2*(current2%2)+current1%2+1
                    queue.append((half1,half2))
                    if half1>=lout and half2<=rout:
                        res="h"
                        not_found=False
                
                if triple2<1000000 and (triple1,triple2) not in seen:
                    seen[(triple1,triple2)]=0
                    queue.append((triple1,triple2))
                    if triple1>=lout and triple2<=rout and not_found:
                        res="t"
                        not_found=False
            
            if not not_found:
                
                while (lin,rin)!=(current1,current2):
                
                    key=seen[(current1,current2)]
                    if key==0:
                        res="t"+res
                        current1=(current1-1)//3
                        current2=(current2-1)//3
                        
                    else:
                        res="h"+res
                        add1=(key-1)%2
                        add2=(key-1)//2
                        current1=2*current1+add1
                        current2=2*current2+add2
            
            else:
                res='IMPOSSIBLE'

    print(res)    
