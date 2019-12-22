#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    
    if (argc!=2)
        return 0;	
    
    int array_length,color_length,i;
    
    FILE *file = fopen(argv[1], "r");
    
    
    fscanf(file,"%d", &array_length);
    fscanf(file,"%d", &color_length);

    int array[array_length];
    for (i=0; i<array_length; i++) 
        fscanf(file,"%d", &array[i]);
    
    if(color_length>array_length)
    {
        printf("The number of colors is greater than the number of parts");
        return 0;
    }
    
    int min_length=array_length+1,first=0,last=-1,colors_used=0,temp_length;
    int color_freq[color_length+1];
    
    
    for (i=0; i<color_length+1; i++)
        color_freq[i]=0;
    
    while (last<array_length)
    {
        if (colors_used!=color_length && last<array_length-1)
        {
            ++last;
            if (color_freq[array[last]]==0)
                ++colors_used;
            ++color_freq[array[last]];
            
        }
        else if (colors_used==color_length)
        {
            temp_length=last-first+1;
            if (temp_length<min_length) 
                min_length=temp_length;
            
            if (color_freq[array[first]]==1)
                --colors_used;
            --color_freq[array[first]];
            ++first;
        }
        else break;
    }
    
    if (min_length==array_length+1) min_length=0;
    printf("%d\n",min_length);
    fclose(file);
    return 0;
}
