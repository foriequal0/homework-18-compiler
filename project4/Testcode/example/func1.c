int x;
int outprint(int* j){
	*j = *j + 10;
}

int main(){
	int i;
	int k;
	int *l;

	k = 10;
	l = &k;
	x=1;
	i = outprint(l);
	write_int(k);
	write_string("\n");
}
