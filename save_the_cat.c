

#include <iostream>
#include <queue>  
#include <fstream>
#include <string>
using namespace std;

char basement[1001][1001];

bool water_can_move_to(char spot)
{
	return (spot=='.' || spot=='A');
}

bool cat_can_move_to(char spot)
{
	return (spot=='.');
}
void print_reversed(string str) 
{ 
	for (int i=str.length()-1; i>=0; i--) 
		cout << str[i];  
	cout<<endl;
} 

int main(int argc, char **argv)
{	
	if(argc!=2)
		return 0;

	ifstream input_file(argv[1]);

	int i,j;
	string line;

	int n=0;
	int m=0;
	while ( getline(input_file,line) )
	{
		if (n==0) m=line.length();
		for (j=0; j<m; j++)
		{
			basement[n][j]=line[j];
		}
		++n;
	}
	input_file.close();

	queue<int> water_coord1,water_coord2,cat_coord1,cat_coord2;

	//This array shows the time water (or cat) reaches each square of the map. -1 means water hasn't reached the square yet
	int water_time[n][m],cat_time[n][m];
	int cat_init1=0,cat_init2=0;//iitial position of the cat
	//Check all squares of the initial map. If there's a water source set the water time to 0 and add the square to the water queues
	for (i=0; i<n; i++)
		for (j=0; j<m; j++)
		{
			if (basement[i][j]=='W') 
			{
				water_time[i][j]=0;
				water_coord1.push(i);
				water_coord2.push(j);
			}
			else water_time[i][j]=-1;
			if (basement[i][j]=='A') 
			{
				cat_time[i][j]=0;
				cat_coord1.push(i);
				cat_coord2.push(j);
				cat_init1=i;
				cat_init2=j;
			}
			else
				cat_time[i][j]=-1;
		}		

	//BFS for water and cat
	int current_time=0,coord1,coord2;

	//stop when the entire basemet is flooded
	while (!water_coord1.empty() || !cat_coord1.empty())
	{
		//move water
		if(!water_coord1.empty() && water_time[water_coord1.front()][water_coord2.front()]==current_time)
		{	
			coord1=water_coord1.front();
			coord2=water_coord2.front();
			//check the square below
			if (coord1!=n-1 && water_can_move_to(basement[coord1+1][coord2]))
			{
				water_time[coord1+1][coord2]=current_time+1;
				basement[coord1+1][coord2]='W';
				water_coord1.push(coord1+1);
				water_coord2.push(coord2);	
			}
			//check the square left
			if (coord2!=0 && water_can_move_to(basement[coord1][coord2-1]))
			{
				water_time[coord1][coord2-1]=current_time+1;
				basement[coord1][coord2-1]='W';
				water_coord1.push(coord1);
				water_coord2.push(coord2-1);	
			}
			//check the square right
			if (coord2!=m-1 && water_can_move_to(basement[coord1][coord2+1]))
			{
				water_time[coord1][coord2+1]=current_time+1;
				basement[coord1][coord2+1]='W';
				water_coord1.push(coord1);
				water_coord2.push(coord2+1);	
			} 
			//check the square above
			if (coord1!=0 && water_can_move_to(basement[coord1-1][coord2]))
			{
				water_time[coord1-1][coord2]=current_time+1;
				basement[coord1-1][coord2]='W';
				water_coord1.push(coord1-1);
				water_coord2.push(coord2);	
			}

			water_coord1.pop();
			water_coord2.pop();
		}
		//else move cat
		else 
		{	
			while (!cat_coord1.empty() && cat_time[cat_coord1.front()][cat_coord2.front()]==current_time)
			{	
				coord1=cat_coord1.front();
				coord2=cat_coord2.front();

				//check the square down	
				if (coord1!=n-1 && cat_can_move_to(basement[coord1+1][coord2]))
				{
					cat_time[coord1+1][coord2]=current_time+1;
					basement[coord1+1][coord2]='A';
					cat_coord1.push(coord1+1);
					cat_coord2.push(coord2);	
				}
				//check the square left
				if (coord2!=0 && cat_can_move_to(basement[coord1][coord2-1]))
				{
					cat_time[coord1][coord2-1]=current_time+1;
					basement[coord1][coord2-1]='A';
					cat_coord1.push(coord1);
					cat_coord2.push(coord2-1);
				}
				//check the square right
				if (coord2!=m-1 && cat_can_move_to(basement[coord1][coord2+1]))
				{
					cat_time[coord1][coord2+1]=current_time+1;
					basement[coord1][coord2+1]='A';
					cat_coord1.push(coord1);
					cat_coord2.push(coord2+1);
				} 
				//check the square above
				if (coord1!=0 && cat_can_move_to(basement[coord1-1][coord2]))
				{
					cat_time[coord1-1][coord2]=current_time+1;
					basement[coord1-1][coord2]='A';
					cat_coord1.push(coord1-1);
					cat_coord2.push(coord2);
				}
				cat_coord1.pop();
				cat_coord2.pop();	
			}
			++current_time;
		}
	}

	int rescue_coord1=cat_init1,rescue_coord2=cat_init2;
	//find the maximum rescue time and the exact square the cat is going to be rescued
	//if the initial position of the cat is not filled with water when the water stops spreading
	// then the cat is protected by obstacles or there's no water whatsoever
	// meaning the cat can be saved anytime so print "infinity"
	if (basement[cat_init1][cat_init2]=='A')
	{	
		//cat can be saved anytime
		//find the exact square where the cast is going to be rescued
		coord1=0;
		coord2=0;
		while(cat_time[coord1][coord2]==-1 && coord1<n)
		{
			if (coord2==m-1)
			{
				++coord1;
				coord2=0;
			}
			else
				++coord2;
		}
		if (coord1==n)
		{
			printf("Error-No initial cat position\n");
			return 0;
		}
		else
		{
			rescue_coord1=coord1;
			rescue_coord2=coord2;
		}

		printf("infinity\n");		
	}
	else //every square accessible to the cat will eventually be flooded
	{	
		int max_rescue_time=-1;

		for (coord1=0; coord1<n; coord1++)
			for (coord2=0; coord2<m; coord2++)
				if(cat_time[coord1][coord2]!=-1 && water_time[coord1][coord2]-1>max_rescue_time)
				{
					max_rescue_time=water_time[coord1][coord2]-1;
					rescue_coord1=coord1;
					rescue_coord2=coord2;
				}

		printf("%d\n", max_rescue_time);
	}


	//at this point both the maximum rescue time and the square where the cat is going to be rescued are known
	//the only thing that remains is to find the exact path the cat has to take
	//if the cat has to remain at her initial position print "stay"
	if (rescue_coord1==cat_init1 && rescue_coord2==cat_init2)
		printf("stay\n");
	else
	{
		coord1=rescue_coord1;
		coord2=rescue_coord2;
		string path="";

		//Now make the string
		coord1=rescue_coord1;
		coord2=rescue_coord2;

		while(!(coord1==cat_init1 && coord2==cat_init2))
		{	
			//one step down
			if (coord1!=n-1 && cat_time[coord1+1][coord2]==cat_time[coord1][coord2]-1)
			{
				++coord1;
				path+='U';
			}
			//one step left
			else if (coord2!=0 && cat_time[coord1][coord2-1]==cat_time[coord1][coord2]-1)
			{
				--coord2;
				path+='R';
			}
			//one step right
			else if (coord2!=m-1 && cat_time[coord1][coord2+1]==cat_time[coord1][coord2]-1)
			{
				++coord2;
				path+='L';
			}
			//one step up
			else if (coord1!=0 && cat_time[coord1-1][coord2]==cat_time[coord1][coord2]-1)
			{
				--coord1;
				path+='D';
			}
			else
			{
				printf("Error while finding the path2\n");
				break;
			}
		}
		print_reversed(path);
	}

	return 0;
}
