#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <time.h>

/* You need to change this macro to your TFS mount point*/
#define TESTDIR "/tmp/netID/mountdir"

#define N_FILES 100
#define BLOCKSIZE 4096
#define FSPATHLEN 256
#define ITERS 16
#define ITERS_LARGE 2048
#define FILEPERM 0666
#define DIRPERM 0755

char buf[BLOCKSIZE];


char *get_base_path(){
	char *current_path = calloc(1000, sizeof(char));

    strcpy(current_path, TESTDIR);

	return current_path;
}

char *make_dir_path_at_depth(int depth){
	char *current_path = get_base_path();

	for(int i = 0; i < depth; i++){
		strcat(current_path, "/files");
	}
	return current_path;
}

void full_test_in_directory(char *parent_dir, int depth){
    int i, fd = 0, ret = 0;
	struct stat st;

	char new_dir[1000];
	char new_file[1000];
	// char *new_subdir[1000];
	strcpy(new_dir, parent_dir);
	strcpy(new_file, parent_dir);
	// strcpy(new_subdir, parent_dir);


	strcat(new_dir, "/files");
	strcat(new_file, "/file");
	// strcat(new_subdir, "/files/dir");
	
	printf("at depth %d and parent dir is %s, new dir: %s, new file: %s \n", depth, parent_dir, new_dir, new_file);


	/* TEST 1: file create test */
	if ((fd = creat(new_file, FILEPERM)) < 0) {
		perror("creat");
		printf("TEST 1: File create failure at depth %d \n", depth);
		exit(1);
	}
	printf("TEST 1: File create Success \n");


	/* TEST 2: file small write test */
	for (i = 0; i < ITERS; i++) {
		//memset with some random data
		memset(buf, 0x61 + i, BLOCKSIZE);

		if (write(fd, buf, BLOCKSIZE) != BLOCKSIZE) {
			printf("TEST 2: File write failure at depth %d \n", depth);
			exit(1);
		}
	}
	
	fstat(fd, &st);
	if (st.st_size != ITERS*BLOCKSIZE) {
		printf("TEST 2: File write failure at depth %d \n", depth);
		exit(1);
	}
	printf("TEST 2: File write Success at depth %d \n", depth);


	/* TEST 3: file close */
	if (close(fd) < 0) {
		printf("TEST 3: File close failure at depth %d \n", depth);
		exit(1);
	}
	printf("TEST 3: File close Success at depth %d \n", depth);


	/* Open for reading */
	if ((fd = open(new_file, FILEPERM)) < 0) {
		perror("open");
		exit(1);
	}

	/* TEST 4: file small read test */
	for (i = 0; i < ITERS; i++) {
		//clear buffer
		memset(buf, 0, BLOCKSIZE);

		if (read(fd, buf, BLOCKSIZE) != BLOCKSIZE) {
			printf("TEST 4: File read failure at depth %d \n", depth);
			exit(1);
		}
		//printf("buf %s \n", buf);
	}
        
	if (pread(fd, buf, BLOCKSIZE, 2*BLOCKSIZE) != BLOCKSIZE) {
		perror("pread");
		printf("TEST 4: File read failure at depth %d \n", depth);
		exit(1);
	}
    
	printf("TEST 4: File read Success at depth %d \n", depth);
	close(fd);


	/* TEST 5: directory create test */
	if ((ret = mkdir(new_dir, DIRPERM)) < 0) {
		perror("mkdir");
		printf("TEST 5: failure. Check if dir %s already exists, and "
			"if it exists, manually remove and re-run AT depth %d \n", TESTDIR "/files", depth);
		exit(1);
	}
	printf("TEST 5: Directory create success at depth %d \n", depth);


	/* TEST 6: sub-directory create test */
	for (i = 0; i < N_FILES; ++i) {
		char subdir_path[1100];
		memset(subdir_path, 0, 1100);

		sprintf(subdir_path, "%s%s%d", new_dir, "/dir", i);
		if ((ret = mkdir(subdir_path, DIRPERM)) < 0) {
			perror("mkdir");
			printf("TEST 6: Sub-directory create failure at depth %d \n", depth);
			exit(1);
		}
	}
	printf("TEST 6: Sub-directory create success at depth %d \n", depth);
	
	for (i = 0; i < N_FILES; ++i) {
		DIR *dir;
		char subdir_path[1100];
		memset(subdir_path, 0, 1100);

		sprintf(subdir_path, "%s%s%d", new_dir, "/dir", i);
		if ((dir = opendir(subdir_path)) == NULL) {
			perror("opendir");
			printf("TEST 7: Sub-directory create failure at depth %d \n", depth);
			exit(1);
		}
	}
	printf("TEST 7: Sub-directory create success at depth %d \n", depth);


	/* Close operation */	
	if (close(fd) < 0) {
		perror("close largefile");
		exit(1);
	}

	printf("Benchmark completed at depth %d \n", depth);
}



void create_deep_directory(int limit){

    for(int i = 0; i < limit; i++){
		char *current_path = make_dir_path_at_depth(i);
        full_test_in_directory(current_path, i);
		free(current_path);
    }
}

void delete_at_depth(int depth){

    char *current_path = make_dir_path_at_depth(depth);

	//if(unlink) maybe try an unlink or something
	int ret;
	if ((ret = rmdir(current_path)) < 0) {
		perror("mkdir");
		printf("failed to recurively delete in directory %s which is depth %d \n", current_path, depth);
		exit(1);
	}

	free(current_path);

}

int main(int argc, char **argv) {
	create_deep_directory(10);
	printf("deep directory created \n");

	delete_at_depth(5);

	DIR *dir;

	char *dir_at_depth = make_dir_path_at_depth(5);

	if ((dir = opendir(dir_at_depth)) != NULL) {
		perror("opendir");
		printf("Somehow opened a dir that should be deleted\n");
		exit(1);
	}

	free(dir_at_depth);

	dir_at_depth = make_dir_path_at_depth(4);

	if ((dir = opendir(dir_at_depth)) == NULL) {
		perror("opendir");
		printf("Couldn't open a dir that shouldn't be deleted\n");
		exit(1);
	}

	free(dir_at_depth);

	printf("delete half way down successful \n");


	delete_at_depth(1); // we can't delete the parent directory of course so not 0

	char *base_path = get_base_path();

	if ((dir = opendir(base_path)) == NULL) {
		perror("opendir");
		printf("Couldn't open a dir that shouldn't be deleted \n");
		exit(1);
	}

	free(base_path);

	dir_at_depth = make_dir_path_at_depth(1);

	if ((dir = opendir(dir_at_depth)) != NULL) {
		perror("opendir");
		printf("Somehow opened a dir that should be deleted \n");
		exit(1);
	}
	
	free(dir_at_depth);

	printf("full depth delete successful \n");

	char *path_to_lone_file = get_base_path();

	strcat(path_to_lone_file, "/file");

	int ret;
	if ((ret = unlink(path_to_lone_file)) < 0) {
		perror("unlink");
		printf("failed to unlink lone file in root dir \n");
		exit(1);
	}

	free(path_to_lone_file);

	printf("deletion of lone file in parent successful \n");

	printf("tests pass \n");

	printf("feel free the check the mount dir, it is empty once again! \n");
}