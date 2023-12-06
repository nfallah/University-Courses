/*
 *  Copyright (C) 2023 CS416 Rutgers CS
 *	Tiny File System
 *	File:	rufs.c
 *
 */

#define FUSE_USE_VERSION 26

#include <fuse.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <sys/time.h>
#include <libgen.h>
#include <limits.h>

#include "block.h"
#include "rufs.h"

char diskfile_path[PATH_MAX];

// Declare your in-memory data structures here
struct superblock *superblock;

/* 
 * Get available inode number from bitmap
 */
int get_avail_ino() {
	// Step 1: Read inode bitmap from disk
	// Step 2: Traverse inode bitmap to find an available slot
	// Step 3: Update inode bitmap and write to disk
	bitmap_t inode_bitmap = get_inode_bitmap(superblock);
	if (!inode_bitmap) return -1;
	size_t inode_bitmap_byte_size = (superblock->max_inum + 7) / 8,
		inode_bitmap_block_size = (inode_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	for (unsigned int i = 0; i < inode_bitmap_byte_size; i++) {
		if (inode_bitmap[i] == 255) continue;
		for (int j = 0; j < 8; j++) {
			if (get_bitmap(inode_bitmap, i * 8 + j) == FALSE) {
				set_bitmap(inode_bitmap, i * 8 + j);
				if (bio_write_multi(superblock->i_bitmap_blk, inode_bitmap_block_size, inode_bitmap) != EXIT_SUCCESS) {
					free(inode_bitmap);
					return -1;
				}
				free(inode_bitmap);
				return i * 8 + j;
			}
		}
	}
	free(inode_bitmap);
	return -1;
}

/* 
 * Get available data block number from bitmap
 */
int get_avail_blkno() {
	// Step 1: Read data block bitmap from disk
	// Step 2: Traverse data block bitmap to find an available slot
	// Step 3: Update data block bitmap and write to disk 
    bitmap_t data_bitmap = get_data_bitmap(superblock);
    if (!data_bitmap) return -1;
    size_t data_bitmap_byte_size = (superblock->max_dnum + 7) / 8,
        data_bitmap_block_size = (data_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
    for (unsigned int i = 0; i < data_bitmap_byte_size; i++) {
		if (data_bitmap[i] == 255) continue;
		for (int j = 0; j < 8; j++) {
			if (get_bitmap(data_bitmap, i * 8 + j) == FALSE) {
				set_bitmap(data_bitmap, i * 8 + j);
				if (bio_write_multi(superblock->d_bitmap_blk, data_bitmap_block_size, data_bitmap) != EXIT_SUCCESS) {
					free(data_bitmap);
					return -1;
				}
				free(data_bitmap);
				return i * 8 + j;
			}
		}
    }
    free(data_bitmap);
    return -1;
}

/* 
 * inode operations
 */
int readi(uint16_t ino, struct inode *inode) {
	// Step 1: Get the inode's on-disk block number
	// Step 2: Get offset of the inode in the inode on-disk block
	// Step 3: Read the block from disk and then copy into inode structure
	if (ino >= superblock->max_inum) return -1;
	size_t inodes_byte_size = superblock->max_inum * sizeof(struct inode),
		inodes_block_size = (inodes_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	void *base = malloc(inodes_block_size * BLOCK_SIZE);
	if (!base) return -1;
	if (bio_read_multi(superblock->i_start_blk, inodes_block_size, base) != EXIT_SUCCESS) {
		free(base);
		return -1;
	}
	memcpy((void *)inode, base + ino * sizeof(struct inode), sizeof(struct inode));
	free(base);
	return EXIT_SUCCESS;
}

int writei(uint16_t ino, struct inode *inode) {
	// Step 1: Get the block number where this inode resides on disk
	// Step 2: Get the offset in the block where this inode resides on disk
	// Step 3: Write inode to disk 
	if (ino >= superblock->max_inum) {
		return -1;
	}
	size_t inodes_byte_size = superblock->max_inum * sizeof(struct inode),
		inodes_block_size = (inodes_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	void *base = malloc(inodes_block_size * BLOCK_SIZE);
	if (!base) return -1;
	if (bio_read_multi(superblock->i_start_blk, inodes_block_size, base) != EXIT_SUCCESS) {
		free(base);
		return -1;
	}
	memcpy(base + ino * sizeof(struct inode), (void *)inode, sizeof(inode));
	if (bio_write_multi(superblock->i_start_blk, inodes_block_size, base) != EXIT_SUCCESS) {
		free(base);
		return -1;
	}
	free(base);
	return EXIT_SUCCESS;
}

/* 
 * directory operations
 */
int dir_find(uint16_t ino, const char *fname, size_t name_len, struct dirent *dirent) {
	// Step 1: Call readi() to get the inode using ino (inode number of current directory)
	// Step 2: Get data block of current directory from inode
	// Step 3: Read directory's data block and check each directory entry.
	// If the name matches, then copy directory entry to dirent structure
	printf("dir_find(): ENTER\n");
	struct inode *inode = malloc(sizeof(struct inode));
	if (!inode) return -1;
	if (readi(ino, inode) != EXIT_SUCCESS) {
		free(inode);
		return -1;
	}
	void *base = malloc(BLOCK_SIZE);
	if (!base) {
		free(inode);
		return -1;
	}
	size_t inode_block_size = (inode->size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	if (inode_block_size > 16 || inode->type != DIRECTORY || !inode->valid) {
		free(inode);
		free(base);
		return -1;
	}
	size_t block_dirent_size = BLOCK_SIZE / sizeof(struct dirent),
		size = inode->size;
	for (unsigned int i = 0; i < inode_block_size; i++) {
		int block_num = inode->direct_ptr[i];
		if (bio_read_multi(block_num, 1, base) != EXIT_SUCCESS) {
			free(inode);
			free(base);
			return -1;
		}
		for (unsigned int j = 0; j < block_dirent_size; j++) {
			if (size < sizeof(struct dirent)) {
				free(inode);
				free(base);
				return -1;
			}
			struct dirent *current_dirent = (struct dirent *)(base + j * sizeof(struct dirent));
			if (current_dirent->valid && strncmp(current_dirent->name, fname, name_len) == 0) {
				memcpy(dirent, current_dirent, sizeof(struct dirent));
				free(inode);
				free(base);
				return EXIT_SUCCESS;
			}
			size = size >= sizeof(struct dirent) ? size - sizeof(struct dirent) : 0;
		}
	}
	free(inode);
	free(base);
	printf("dir_find(): EXIT\n");
	return -1;
}

int dir_add(struct inode dir_inode, uint16_t f_ino, const char *fname, size_t name_len) {
	// Step 1: Read dir_inode's data block and check each directory entry of dir_inode
	// Step 2: Check if fname (directory name) is already used in other entries
	// Step 3: Add directory entry in dir_inode's data block and write to disk
	// Allocate a new data block for this directory if it does not exist
	// Update directory inode
	// Write directory entry
	printf("dir_add(): ENTER\n");
	size_t inode_block_size = (dir_inode.size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	if (inode_block_size > 16 || dir_inode.type != DIRECTORY || !dir_inode.valid) return -1;
	void *base = malloc(BLOCK_SIZE);
	if (!base) return -1;
	size_t block_dirent_size = BLOCK_SIZE / sizeof(struct dirent),
		size = dir_inode.size;
	int block_num_target = -1,
		dirent_index = -1;
	for (unsigned int i = 0; i < inode_block_size; i++) {
		int block_num = dir_inode.direct_ptr[i];
		if (bio_read_multi(block_num, 1, base) != EXIT_SUCCESS) {
			free(base);
			return -1;
		}
		for (unsigned int j = 0; j < block_dirent_size; j++) {
			if (size < sizeof(struct dirent)) goto end;
			struct dirent *current_dirent = (struct dirent *)(base + j * sizeof(struct dirent));
			if (current_dirent->valid && strncmp(current_dirent->name, fname, name_len) == 0) {
				free(base);
				return -1;
			}
			if (!current_dirent->valid && block_num_target == -1) {
				block_num_target = i;
				dirent_index = j;
			}
			size = size >= sizeof(struct dirent) ? size - sizeof(struct dirent) : 0;
		}
	}
	end:
	boolean allocated = FALSE;
	if (block_num_target == -1) {
		if (inode_block_size >= 16) {
			free(base);
			return -1;
		}
		int new_block_num = get_avail_blkno();
		if (new_block_num == -1) {
			free(base);
			return -1;
		}
		memset(base, 0, BLOCK_SIZE);
		dir_inode.size += BLOCK_SIZE;
		dir_inode.direct_ptr[inode_block_size] = new_block_num;
		block_num_target = inode_block_size;
		dirent_index = 0;
		allocated = TRUE;
	}
	if (block_num_target < block_dirent_size - 1 && bio_read_multi(block_num_target, 1, base) != EXIT_SUCCESS) {
		free(base);
		return -1;
	}
	dir_inode.link++;
	if (writei(dir_inode.ino, &dir_inode) != EXIT_SUCCESS) {
		free(base);
		return -1;
	}
	struct dirent *dirent = (struct dirent *)(base + dirent_index * sizeof(struct dirent));
	dirent->ino = f_ino;
	dirent->valid = TRUE;
	memset(dirent->name, 0, 208);
	memcpy(dirent->name, fname, name_len + 1); // name_len does *not* account for null terminator
	dirent->len = name_len;
	if (bio_write_multi(block_num_target, 1, base) != EXIT_SUCCESS) {
		dir_inode.link--;
		if (allocated) {
			bitmap_t data_bitmap = get_data_bitmap(superblock);
			unset_bitmap(data_bitmap, block_num_target);
			update_data_bitmap(data_bitmap, TRUE, superblock);
			dir_inode.size -= BLOCK_SIZE;
			dir_inode.direct_ptr[block_num_target] = 0;
		}
		writei(dir_inode.ino, &dir_inode);
		free(base);
		return -1;
	}
	free(base);
	printf("dir_add(): EXIT\n");
	return EXIT_SUCCESS;
}

int dir_remove(struct inode dir_inode, const char *fname, size_t name_len) {
	// Step 1: Read dir_inode's data block and checks each directory entry of dir_inode
	// Step 2: Check if fname exist
	// Step 3: If exist, then remove it from dir_inode's data block and write to disk
	return EXIT_SUCCESS;
}

/* 
 * namei operation
 */
int get_node_by_path(const char *path, uint16_t ino, struct inode *inode) {
    printf("get_node_by_path(): entered!\n");
    printf("get_node_by_path() target: %s\n", path);

    if (path[0] == '\0') {
        printf("Empty input path\n");
        return -1;
    }

    char *path_copy = strdup(path);
    if (!path_copy) return -1;

	struct dirent *current_dirent = malloc(sizeof(struct dirent));
	if (!current_dirent) {
		free(path_copy);
		return -1;
	}

    printf("get_node_by_path(): starting target_directory is %s\n", path_copy);

    if (path_copy[0] != '/') {
        free(path_copy);
		free(current_dirent);
        return -1;
    }

    int current_ino = ino; // Inode number of the root directory
    char *target_directory = path_copy + 1; // Start after the initial '/'
	memset(current_dirent, 0, sizeof(struct dirent));

    while (*target_directory != '\0') {
        // Find the end of the current path component
        char *end = target_directory;
        while (*end != '/' && *end != '\0') {
            end++;
        }

        // Check if the current path component is empty
        if (target_directory == end) {
            // Handle consecutive '/' characters as an empty string
            target_directory = end + 1;
            continue;
        }

        // Perform directory lookup using the current path component
        if (dir_find(current_ino, target_directory, end - target_directory, current_dirent) != EXIT_SUCCESS) {
            free(path_copy);
			free(current_dirent);
            return -1;
        }

        current_ino = current_dirent->ino;

        // Move to the next path component
        target_directory = (*end == '/') ? (end + 1) : end;
    }

    printf("final ino: %d\n", current_ino);

    if (readi(current_ino, inode) != EXIT_SUCCESS) {
        free(path_copy);
		free(current_dirent);
        return -1;
    }

    free(path_copy);
	free(current_dirent);
    printf("get_node_by_path(): END\n");
    return EXIT_SUCCESS;
}

/* 
 * Make file system
 */
int rufs_mkfs() {
	printf("rufs_mkfs(): START\n");
	// Call dev_init() to initialize (Create) Diskfile
	// write superblock information
	// initialize inode bitmap
	// initialize data block bitmap
	// update bitmap information for root directory
	// update inode for root directory

	/* Hierarchy of blocks:
	 * 1) Superblock
	 * 2) Inode bitmap
	 * 3) Data bitmap
	 * 4) Inodes
	 * 5) Data
	 */

	dev_init(diskfile_path);
	// Superblock initialization
	size_t superblock_block_size = (sizeof(struct superblock) + BLOCK_SIZE - 1) / BLOCK_SIZE;
	struct superblock *superblock = malloc(superblock_block_size * BLOCK_SIZE);
	memset(superblock, 0, superblock_block_size * BLOCK_SIZE);
	superblock->magic_num = MAGIC_NUM;
	superblock->max_inum = MAX_INUM;
	superblock->max_dnum = MAX_DNUM;
	unsigned int block_num = superblock_block_size;
	// Inode bitmap initialization
	superblock->i_bitmap_blk = block_num;
	size_t inode_bitmap_byte_size = (MAX_INUM + 7) / 8,
		inode_bitmap_block_size = (inode_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t inode_bitmap = malloc(inode_bitmap_block_size * BLOCK_SIZE);
	memset(inode_bitmap, 0, inode_bitmap_block_size * BLOCK_SIZE);
	block_num += inode_bitmap_block_size;
	// Data bitmap initialization
	superblock->d_bitmap_blk = block_num;
	size_t data_bitmap_byte_size = (MAX_DNUM + 7) / 8,
		data_bitmap_block_size = (data_bitmap_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	bitmap_t data_bitmap = malloc(data_bitmap_block_size * BLOCK_SIZE);
	memset(data_bitmap, 0, data_bitmap_block_size * BLOCK_SIZE);
	block_num += data_bitmap_block_size;
	// Inodes initialization
	superblock->i_start_blk = block_num;
	size_t inodes_byte_size = MAX_INUM * sizeof(struct inode),
		inodes_block_size = (inodes_byte_size + BLOCK_SIZE - 1) / BLOCK_SIZE;
	unsigned char *inodes = malloc(inodes_block_size * BLOCK_SIZE);
	memset(inodes, 0, inodes_block_size * BLOCK_SIZE);
	block_num += inodes_block_size;
	// Data initialization
	superblock->d_start_blk = block_num;
	// Update data bitmap
	for (unsigned int current_block_num = 0; current_block_num < block_num; current_block_num++) {
		set_bitmap(data_bitmap, current_block_num);
	}
	// Initialize root directory
	struct inode *rootdir_inode = (struct inode *)inodes;
	set_bitmap(inode_bitmap, 0);
	rootdir_inode->ino = 0;
	rootdir_inode->size = BLOCK_SIZE;
	rootdir_inode->type = DIRECTORY;
	rootdir_inode->valid = TRUE;
	rootdir_inode->vstat.st_mode = S_IFDIR | 0755;
	rootdir_inode->vstat.st_atime = rootdir_inode->vstat.st_mtime = time(NULL);
	// Write data to disk
	if (bio_write_multi(0, superblock_block_size, superblock) != EXIT_SUCCESS) return EXIT_FAILURE;
	if (bio_write_multi(superblock->i_bitmap_blk, inode_bitmap_block_size, inode_bitmap) != EXIT_SUCCESS) return EXIT_FAILURE;
	if (bio_write_multi(superblock->d_bitmap_blk, data_bitmap_block_size, data_bitmap) != EXIT_SUCCESS) return EXIT_FAILURE;
	if (bio_write_multi(superblock->i_start_blk, inodes_block_size, inodes) != EXIT_SUCCESS) return EXIT_FAILURE;
	free(superblock);
	free(inode_bitmap);
	free(data_bitmap);
	free(inodes);
	printf("rufs_mkfs(): END!\n");
	return EXIT_SUCCESS;
}

/* 
 * FUSE file operations
 */
static void *rufs_init(struct fuse_conn_info *conn) {
	// Step 1a: If disk file is not found, call mkfs
	// Step 1b: If disk file is found, just initialize in-memory data structures
	// and read superblock from disk
	boolean init = FALSE;
	printf("rufs_init(): START\n");
	if (dev_open(diskfile_path) == -1) {
		printf("rufs_init(): calling rufs_mkfs()\n");
		if (rufs_mkfs() != EXIT_SUCCESS) return NULL;
		init = TRUE;
	}
	if (!(superblock = get_superblock())) {
		dev_close(diskfile_path);
		return NULL;
	}
	if (init) {
		printf("rufs_init(): start of init\n");
		struct inode *root_directory = malloc(sizeof(struct inode));
		memset(root_directory, 0, sizeof(struct inode));
		readi(0, root_directory);
		dir_add(*root_directory, 0, ".", 1);
		dir_add(*root_directory, 0, "..", 2);
		free(root_directory);
		printf("rufs_init(): end of init\n");
	}
	printf("rufs_init(): END\n");
	return NULL;
}

static void rufs_destroy(void *userdata) {
	// Step 1: De-allocate in-memory data structures
	// Step 2: Close diskfile
	printf("rufs_destroy(): CALLED!\n");
	free(superblock);
	dev_close(diskfile_path);
}

static int rufs_getattr(const char *path, struct stat *stbuf) {
	// Step 1: call get_node_by_path() to get inode from path
	// Step 2: fill attribute of file into stbuf from inode
	printf("rufs_getattr(): called\n");
	struct inode *inode = malloc(sizeof(struct inode));
	if (!inode) {
		return -ENOMEM;
	}
	if (get_node_by_path(path, ROOTDIR, inode) != EXIT_SUCCESS) {
		return -ENOENT;
	}
	memset(stbuf, 0, sizeof(struct stat));
	/*
	if (inode->type == DIRECTORY) {
		stbuf->st_mode = S_IFDIR | 0755;
	} else {
		stbuf->st_mode = S_IFREG | 0755;
	}*/
	stbuf->st_mode = inode->vstat.st_mode;
	stbuf->st_nlink = inode->link;
	stbuf->st_uid = getuid();
	stbuf->st_gid = getgid();
	stbuf->st_size = inode->size;
	time(&stbuf->st_mtime);
	free(inode);
	return 0;
	// Supposed to update inode time here as well??
}

static int rufs_opendir(const char *path, struct fuse_file_info *fi) {
	// Step 1: Call get_node_by_path() to get inode from path
	// Step 2: If not find, return -1
	printf("rufs_opendir(): called\n");
	struct inode *inode = malloc(sizeof(struct inode));
	if (!inode) {
		return -ENOMEM;
	}
	if (get_node_by_path(path, ROOTDIR, inode) != EXIT_SUCCESS) {
		free(inode);
		return -1;
	}
	if (inode->type != DIRECTORY) {
		free(inode);
		return -ENOTDIR;
	}
	free(inode);
    return 0;
}

static int rufs_readdir(const char *path, void *buffer, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fi) {
	// Step 1: Call get_node_by_path() to get inode from path
	// Step 2: Read directory entries from its data blocks, and copy them to filler
	printf("rufs_readdir(): called\n");
	struct inode *inode = malloc(sizeof(struct inode));
	if (!inode) {
		return -ENOMEM;
	}
	void *base = malloc(BLOCK_SIZE);
	if (!base) {
		free(inode);
		return -ENOMEM;
	}
    if (get_node_by_path(path, ROOTDIR, inode) != EXIT_SUCCESS) {
        free(inode);
		free(base);
        return -ENOENT;
    }
	if (inode->type != DIRECTORY) {
		free(inode);
		free(base);
		return -ENOTDIR;
	}
	size_t inode_block_size = (inode->size + BLOCK_SIZE - 1) / BLOCK_SIZE,
		block_dirent_size = BLOCK_SIZE / sizeof(struct dirent),
		size = inode->size;
	for (unsigned int i = 0; i < inode_block_size; i++) {
		int block_num = inode->direct_ptr[i];
		if (bio_read_multi(block_num, 1, base) != EXIT_SUCCESS) {
			free(inode);
			free(base);
			return -EIO;
		}
		for (unsigned int j = 0; j < block_dirent_size; j++) {
			if (size < sizeof(struct dirent)) {
				goto end;
			}
			struct dirent *current_dirent = (struct dirent *)(base + j * sizeof(struct dirent));
			filler(buffer, current_dirent->name, NULL, 0);
			size = size >= sizeof(struct dirent) ? size - sizeof(struct dirent) : 0;
		}
	}
	end:
		free(inode);
		free(base);
		return 0;
}

static int rufs_mkdir(const char *path, mode_t mode) {
	// Step 1: Use dirname() and basename() to separate parent directory path and target directory name
	// Step 2: Call get_node_by_path() to get inode of parent directory
	// Step 3: Call get_avail_ino() to get an available inode number
	// Step 4: Call dir_add() to add directory entry of target directory to parent directory
	// Step 5: Update inode for target directory
	// Step 6: Call writei() to write inode to disk
	printf("rufs_mkdir(): called\n");
	char *path_copy = strdup(path);
	if (!path_copy) {
		return -ENOMEM;
	}
	struct inode *dir_inode = malloc(sizeof(struct inode));
	if (!dir_inode) {
		free(path_copy);
		return -ENOMEM;
	}
	struct inode *base_inode = malloc(sizeof(struct inode));
	if (!base_inode) {
		free(path_copy);
		free(dir_inode);
		return -ENOMEM;
	}
	void *block_ptr = malloc(BLOCK_SIZE);
	if (!block_ptr) {
		free(path_copy);
		free(dir_inode);
		free(base_inode);
		return -ENOMEM;
	}
	char *dir_path = dirname(path_copy);
	char *base = basename(path_copy); // Possible error due to implementation
	if (get_node_by_path(dir_path, ROOTDIR, dir_inode) != EXIT_SUCCESS) {
		free(path_copy);
		free(dir_inode);
		free(base_inode);
		free(block_ptr);
		return -ENOENT;
	}
	int base_ino, base_block;
	if ((base_ino = get_avail_ino()) == -1) {
		free(path_copy);
		free(dir_inode);
		free(base_inode);
		free(block_ptr);
		return -ENOSPC; // Is this the correct flag?
	}
	if ((base_block = get_avail_blkno()) == -1) {
		bitmap_t inode_bitmap = get_inode_bitmap(superblock);
		unset_bitmap(inode_bitmap, base_ino);
		update_inode_bitmap(inode_bitmap, TRUE, superblock);
		free(path_copy);
		free(dir_inode);
		free(base_inode);
		free(block_ptr);
		return -ENOSPC;
	}
	if (dir_add(*dir_inode, base_ino, base, strlen(base)) == -1) {
		bitmap_t inode_bitmap = get_inode_bitmap(superblock);
		unset_bitmap(inode_bitmap, base_ino);
		update_inode_bitmap(inode_bitmap, TRUE, superblock);
		bitmap_t data_bitmap = get_data_bitmap(superblock);
		unset_bitmap(data_bitmap, base_block);
		update_data_bitmap(data_bitmap, TRUE, superblock);
		free(path_copy);
		free(dir_inode);
		free(base_inode);
		free(block_ptr);
		return -ENOSPC;
	}
	// Should the vstat times for the parent inode also be updated?
	memset(base_inode, 0, sizeof(struct inode));
	memset(block_ptr, 0, BLOCK_SIZE);
	bio_write(base_block, block_ptr);
	base_inode->link = 0;
	base_inode->direct_ptr[0] = base_block;
	base_inode->ino = base_ino;
	base_inode->size = BLOCK_SIZE;
	base_inode->type = DIRECTORY;
	base_inode->valid = TRUE;
	base_inode->vstat.st_mode = S_IFDIR | mode; // Does MODE already contain S_IFDIR? Doesn't hurt to keep ig
	time_t current_time = time(NULL);
	base_inode->vstat.st_mtime = current_time;
	base_inode->vstat.st_atime = current_time;
	readi(dir_inode->ino, dir_inode);
	dir_inode->vstat.st_mtime = current_time;
	writei(dir_inode->ino, dir_inode);
	writei(base_ino, base_inode);
	dir_add(*base_inode, base_ino, ".", 1);
	dir_add(*base_inode, dir_inode->ino, "..", 2);
	free(path_copy);
	free(dir_inode);
	free(base_inode);
	free(block_ptr);
	return 0;
}

static int rufs_rmdir(const char *path) {
	// Step 1: Use dirname() and basename() to separate parent directory path and target directory name
	// Step 2: Call get_node_by_path() to get inode of target directory
	// Step 3: Clear data block bitmap of target directory
	// Step 4: Clear inode bitmap and its data block
	// Step 5: Call get_node_by_path() to get inode of parent directory
	// Step 6: Call dir_remove() to remove directory entry of target directory in its parent directory
	return 0;
}

static int rufs_releasedir(const char *path, struct fuse_file_info *fi) {
	// For this project, you don't need to fill this function
	// But DO NOT DELETE IT!
    return 0;
}

static int rufs_create(const char *path, mode_t mode, struct fuse_file_info *fi) {
	// Step 1: Use dirname() and basename() to separate parent directory path and target file name
	// Step 2: Call get_node_by_path() to get inode of parent directory
	// Step 3: Call get_avail_ino() to get an available inode number
	// Step 4: Call dir_add() to add directory entry of target file to parent directory
	// Step 5: Update inode for target file
	// Step 6: Call writei() to write inode to disk
	printf("rufs_create(): entered\n");
	printf("rufs_create() path is %s\n", path);
	char *path_copy = strdup(path);
	if (!path_copy) {
		return -ENOMEM;
	}
	printf("rufs_create(): a\n");
	struct inode *dir_inode = malloc(sizeof(struct inode));
	if (!dir_inode) {
		free(path_copy);
		return -ENOMEM;
	}
	printf("rufs_create(): b\n");
	struct inode *base_inode = malloc(sizeof(struct inode));
	if (!base_inode) {
		free(path_copy);
		free(dir_inode);
		return -ENOMEM;
	}
	char *dir_path = dirname(path_copy);
	char *base = basename(path_copy); // Possible error due to implementation
	printf("rufs_create(): c\n");
	if (get_node_by_path(dir_path, ROOTDIR, dir_inode) != EXIT_SUCCESS) {
		free(path_copy);
		free(dir_inode);
		free(base_inode);
		return -ENOENT;
	}
	printf("rufs_create(): d\n");
	int base_ino;
	if ((base_ino = get_avail_ino()) == -1) {
		free(path_copy);
		free(dir_inode);
		free(base_inode);;
		return -ENOSPC; // Is this the correct flag?
	}
	if (dir_add(*dir_inode, base_ino, base, strlen(base)) == -1) {
		bitmap_t inode_bitmap = get_inode_bitmap(superblock);
		unset_bitmap(inode_bitmap, base_ino);
		update_inode_bitmap(inode_bitmap, TRUE, superblock);
		free(path_copy);
		free(dir_inode);
		free(base_inode);
		return -ENOSPC;
	}
	// Should the vstat times for the parent inode also be updated?
	memset(base_inode, 0, sizeof(struct inode));
	base_inode->link = 1;
	base_inode->ino = base_ino;
	base_inode->size = 0;
	base_inode->type = FILE;
	base_inode->valid = TRUE;
	base_inode->vstat.st_mode = S_IFREG | mode; // Does MODE already contain S_IFREG? Doesn't hurt to keep ig
	time_t current_time = time(NULL);
	base_inode->vstat.st_mtime = current_time;
	base_inode->vstat.st_atime = current_time;
	readi(dir_inode->ino, dir_inode);
	dir_inode->vstat.st_mtime = current_time;
	writei(dir_inode->ino, dir_inode);
	writei(base_ino, base_inode);
	free(path_copy);
	free(dir_inode);
	free(base_inode);
	printf("rufs_create(): DONE!\n");
	return 0;
}

static int rufs_open(const char *path, struct fuse_file_info *fi) {
	// Step 1: Call get_node_by_path() to get inode from path
	// Step 2: If not find, return -1
	printf("rufs_open(): entered!\n");
	struct inode *inode = malloc(sizeof(struct inode));
	if (!inode) return -1;
	if (get_node_by_path(path, ROOTDIR, inode) != EXIT_SUCCESS || inode->type != FILE) {
		free(inode);
		return -1;
	}
	printf("rufs_open(): success (postnodepath)!\n");
	free(inode);
    return 0;
}

static int rufs_read(const char *path, char *buffer, size_t size, off_t offset, struct fuse_file_info *fi) {
	// Step 1: You could call get_node_by_path() to get inode from path
	// Step 2: Based on size and offset, read its data blocks from disk
	// Step 3: copy the correct amount of data from offset to buffer
	// Note: this function should return the amount of bytes you copied to buffer
	printf("rufs_read(): called\n");
	if (size == 0) return 0;
	struct inode *inode = malloc(sizeof(struct inode));
	if (!inode) return 0;
	char *block_buffer = malloc(BLOCK_SIZE);
	if (!block_buffer) {
		free(inode);
		return 0;
	}
	if (get_node_by_path(path, ROOTDIR, inode) != EXIT_SUCCESS || inode->type != FILE) {
		free(inode);
		free(block_buffer);
		return 0;
	}
	memset(block_buffer, 0, BLOCK_SIZE);
	int starting_block_index = offset / BLOCK_SIZE;
	int ending_block_index = min(15, (offset + size) / BLOCK_SIZE);
	if (ending_block_index - starting_block_index < 0) {
		free(inode);
		free(block_buffer);
		return 0;
	}
	int bytes_left = size,
		block_offset = offset % BLOCK_SIZE;
	for (int i = starting_block_index; i <= ending_block_index; i++) {
		int amount_to_read = min(bytes_left, BLOCK_SIZE - block_offset);
		bytes_left = min(0, bytes_left - amount_to_read);
		if (inode->direct_ptr[i] == 0) {
			memset(block_buffer, 0, BLOCK_SIZE);
		} else {
			bio_read_multi(inode->direct_ptr[i], 1, block_buffer); // ?
		}
		memcpy(buffer + (size - bytes_left), block_buffer + block_offset, amount_to_read);
		bio_write_multi(inode->direct_ptr[i], 1, block_buffer); // ?
		block_offset = 0;
	}
	return size - bytes_left;
}

static int rufs_write(const char *path, const char *buffer, size_t size, off_t offset, struct fuse_file_info *fi) {
	// Step 1: You could call get_node_by_path() to get inode from path
	// Step 2: Based on size and offset, read its data blocks from disk
	// Step 3: Write the correct amount of data from offset to disk
	// Step 4: Update the inode info and write it to disk
	// Note: this function should return the amount of bytes you write to disk
	printf("rufs_write(): called\n");
	if (size == 0) return 0;
	struct inode *inode = malloc(sizeof(struct inode));
	if (!inode) return 0;
	char *block_buffer = malloc(BLOCK_SIZE);
	if (!block_buffer) {
		free(inode);
		return 0;
	}
	bitmap_t data_bitmap = get_data_bitmap(superblock);
	if (!data_bitmap) {
		free(inode);
		free(block_buffer);
		return 0;
	}
	if (get_node_by_path(path, ROOTDIR, inode) != EXIT_SUCCESS || inode->type != FILE) {
		free(inode);
		free(block_buffer);
		free(data_bitmap);
		return 0;
	}
	memset(block_buffer, 0, BLOCK_SIZE);
	int starting_block_index = offset / BLOCK_SIZE;
	int ending_block_index = min(15, (offset + size) / BLOCK_SIZE);
	if (ending_block_index - starting_block_index < 0) {
		free(inode);
		free(block_buffer);
		free(data_bitmap);
		return 0;
	}
	int size_increase = 0;
	for (int i = starting_block_index; i <= ending_block_index; i++) {
		if (inode->direct_ptr[i] == 0) {
			int blkno = get_avail_blkno_no_wr(data_bitmap, superblock);
			if (blkno == -1) {
				free(inode);
				free(block_buffer);
				free(data_bitmap);
				return 0;
			}
			size_increase += BLOCK_SIZE;
			inode->direct_ptr[i] = blkno;
			bio_write_multi(blkno, 1, block_buffer); // Clears all junk data
		}
	}
	if (size_increase > 0) {
		inode->size += size_increase;
		writei(inode->ino, inode);
		update_data_bitmap(data_bitmap, TRUE, superblock);
	} else {
		free(data_bitmap);
	}
	int bytes_left = size,
		block_offset = offset % BLOCK_SIZE;
	for (int i = starting_block_index; i <= ending_block_index; i++) {
		int amount_to_read = min(bytes_left, BLOCK_SIZE - block_offset);
		bytes_left = min(0, bytes_left - amount_to_read);
		bio_read_multi(inode->direct_ptr[i], 1, block_buffer); // ?
		memcpy(block_buffer + block_offset, buffer + (size - bytes_left), amount_to_read);
		bio_write_multi(inode->direct_ptr[i], 1, block_buffer); // ?
		block_offset = 0;
	}
	return size - bytes_left;
}

static int rufs_unlink(const char *path) {
	// Step 1: Use dirname() and basename() to separate parent directory path and target file name
	// Step 2: Call get_node_by_path() to get inode of target file
	// Step 3: Clear data block bitmap of target file
	// Step 4: Clear inode bitmap and its data block
	// Step 5: Call get_node_by_path() to get inode of parent directory
	// Step 6: Call dir_remove() to remove directory entry of target file in its parent directory
	return 0;
}

static int rufs_truncate(const char *path, off_t size) {
	// For this project, you don't need to fill this function
	// But DO NOT DELETE IT!
    return 0;
}

static int rufs_release(const char *path, struct fuse_file_info *fi) {
	// For this project, you don't need to fill this function
	// But DO NOT DELETE IT!
	return 0;
}

static int rufs_flush(const char * path, struct fuse_file_info * fi) {
	// For this project, you don't need to fill this function
	// But DO NOT DELETE IT!
    return 0;
}

static int rufs_utimens(const char *path, const struct timespec tv[2]) {
	// For this project, you don't need to fill this function
	// But DO NOT DELETE IT!
    return 0;
}

static struct fuse_operations rufs_ope = {
	.init		= rufs_init,
	.destroy	= rufs_destroy,

	.getattr	= rufs_getattr,
	.readdir	= rufs_readdir,
	.opendir	= rufs_opendir,
	.releasedir	= rufs_releasedir,
	.mkdir		= rufs_mkdir,
	.rmdir		= rufs_rmdir,

	.create		= rufs_create,
	.open		= rufs_open,
	.read 		= rufs_read,
	.write		= rufs_write,
	.unlink		= rufs_unlink,

	.truncate   = rufs_truncate,
	.flush      = rufs_flush,
	.utimens    = rufs_utimens,
	.release	= rufs_release
};

int main(int argc, char *argv[]) {
	int fuse_stat;
	getcwd(diskfile_path, PATH_MAX);
	strcat(diskfile_path, "/DISKFILE");
	fuse_stat = fuse_main(argc, argv, &rufs_ope, NULL);
	return fuse_stat;
}