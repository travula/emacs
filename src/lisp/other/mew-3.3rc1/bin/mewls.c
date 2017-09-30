/*
 * mewls -- Message scanner for Mew
 *
 * Author:  Kazu Yamamoto <Kazu@Mew.org>
 * Created: Jul  7, 2000
 * Revised: Oct 31, 2001
 *
 * Code:
 */

#include "mew.h"

private char version_message[] = "version 3.2 20030217 Kazu Yamamoto";

#ifdef HAVE_UNISTD_H
# include <sys/types.h>
# include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>

#ifdef HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else /* HAVE_DIRENT_H */
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif /* HAVE_SYS_NDIR_H */
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif /* HAVE_SYS_DIR_H */
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif /* HAVE_NDIR_H */
#endif /* HAVE_DIRENT_H */
#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif

/* for chdir() */
#ifdef HAVE_DIR_H
# include <dir.h>
#endif /* HAVE_DIR_H */

#ifdef HAVE_PWD_H
# include <pwd.h>
#endif /* HAVE_PWD_H */

/****************************************************************
 * 
 * Macro, structure, and tables
 *
 */

#define MAILHOME "Mail"
#define CHDIR    "CD:"
#define INBOX    "+inbox"
#define STDINFILE "-"
#define HEAD     "head"

/* 16k, my experience says that 8k is not enough. */
#define MAX_HEADER         16384
#define MAX_FIELD_LEN          5
#define MAX_FOLDER            16
#define NUMBER_OF_FILE       128
#define MAX_ARGC	      16

#ifdef PATH_MAX
# define MAX_FILE_NAME_LEN    PATH_MAX
#else
# define MAX_FILE_NAME_LEN    1024
#endif

#define DEFAULT_FIELDS \
	"To:,From:,Subject:,Date:,Content-Type:,Message-ID:,X-Mew-UIDL:,In-Reply-To:,References:"

#define REFERENCES "References:"

struct fld_rng {
	char *folder;
	unsigned int first;
	unsigned int last;
	unsigned int num;
};

private char Buf_header[MAX_HEADER];
private char Buf_filenm[MAX_FILE_NAME_LEN];
private char *Mail_home;
private char Current_folder[MAX_FILE_NAME_LEN];
private struct fld_rng Folders[MAX_FOLDER];
private char *Argvec[MAX_ARGC];

private char *Key_field = NULL;
private char *Exec_cmd = NULL;
private char *Exec_arg = NULL;
private char *Input_file = NULL;

private int All_fields = 0;
private int Field_len = MAX_FIELD_LEN;
private int PrintNumOfMsg = NO;

/****************************************************************
 *
 * prototype
 *
 */

private void usage(const char *);
private void help(const char *);
private void version(const char *);
private void ch_home(void);
private void ch_mail_home(char *);
private void ch_folder(char *);
private char *nextline(char *, char *);
private char *nextfield(char *, char *, char **, char **);
private char *eoh(char *, char *);
private void init_fields(char *);
private void print_field(char *, int, char *, char *);
private void init_search(char *, int);
private char *mystrcasechr(char *, int);
private char *mystrcasestr(char *, char *);
private int  print_for_scan(FILE *, char *, char *);
private int  print_for_sort(FILE *, char *, char *);
private int  print_for_pick(FILE *, char *, char *);
private int  scan_numfile(char *, unsigned int, unsigned int);
private int  scan_intcompare(unsigned int *, unsigned int *);
private int  scan_folder(struct fld_rng *);
private void scan_init(unsigned int);
private FILE *scan_getfile(char **, char **);
private void exec_init(unsigned int);
private FILE *exec_getfile(char **, char **);
private void stdin_init(unsigned int);
private void set_fld_rng(struct fld_rng *, char *, char *);

/****************************************************************
 * 
 * options and usages
 *
 */

private void
usage(const char *progname) {
	fprintf(stderr, "Usage: %s [options] [folder [range]]\n", progname);
}

private const char *
help_message[] = {
	" -a            Print all necessary fields when picking.",
	" -b  <mdir>    Mail home.",
	" -d  <field>   A field to be extracted.",
	" -e  <command> An external command to fetch mailbox.",
	" -m  <options> The second argument for the external command.",
	" -f  <fields>  Fields to display.",
	" -h            Display this help message.",
	" -i  <file>    A file name to be read.",
	" -l  <length>  Field max length to dump. (0 means no limit)",
	" -n            Don't use fstat().",
	" -p  <pattern> Pick pattern.",
	" -s  <src>     Message source: '+folder range'.",
	" -w  <time>    Wait_for timer (microsecond).",
	" -v            Display the version.",
	"",
	"<range> = N | [start]-[end] | last:N", 
	"<pattern> = 'key=val' | 'key!=val' | '!<pattern>' | '(<pattern>)' ",
	"          | '<pattern>&<pattern>' ",
	"          | '<pattern>|<pattern>' ",
	"",
	"Default is mdir = ~/Mail,",
	"length = 5, folder = +inbox, range = 0",
	"field = To:,From:,Subject:,Date:,Content-Type:,Message-ID:,X-Mew-UIDL:,In-Reply-To:,References:",
	NULL
};

private void
help(const char *progname) {
	const char **p = help_message;

	fprintf(stderr, "Help: %s\n\n", progname);
	fprintf(stderr, " Message scanner.\n\n");
	usage(progname);
	while (*p) fprintf(stderr, "%s\n", *p++);
}

private void
version(const char *progname) {
	fprintf(stderr, "%s %s\n", progname, version_message);
}

/****************************************************************
 * 
 * folder function
 *
 */

private void
ch_home()
{
	char *home = getenv("HOME");
#ifdef HAVE_GETPWUID
	if(home == NULL || home[0] == NUL) {
		struct passwd *pw = getpwuid(getuid());
		if(pw == NULL)
			warn_exit("failed in getting home directory.\n");
		home = pw->pw_dir;
	}
#endif /* HAVE_GETPWUID */
	if (home == NULL)
		warn_exit("can't change directory to home.\n");
	if (chdir(home) != 0)
		warn_exit("can't change directory to %s.\n", home);
}

private void
ch_mail_home(char *dir)
{
	char *p = dir + 1, c;

	if (dir == NULL) warn_exit("%s not exist.\n", dir);
	c = *dir;
	switch (c) {
	case '~':
		ch_home();
		if (*p == NUL)
			break;
		if (*p++ != FILESEP)
			warn_exit("can't change directory to %s.\n", dir);
		if (*p == NUL)
			break;
		if (chdir(p) != 0)
			warn_exit("can't change directory to %s.\n", dir);
		break;
	case FILESEP:
		if (chdir(dir) != 0)
			warn_exit("can't change directory to %s.\n", dir);
		break;
	default:
		ch_home();
		if (chdir(dir) != 0)
			warn_exit("can't change directory to %s.\n", dir);
		break;
	}
}

private void
ch_folder(char *folder)
{
	char c = *folder, *p = folder + 1;
	switch (c) {
	case '+':
		ch_mail_home(Mail_home);
		if (p == NUL)
			break;
		if (chdir(p) != 0)
			warn_exit("can't change folder to %s.\n", folder);
		break;
	case '~':
		ch_home();
		if (*p == NUL)
			break;
		if (*p++ != FILESEP)
			warn_exit("can't change folder to %s.\n", folder);
		if (*p == NUL)
			break;
		if (chdir(p) != 0)
			warn_exit("can't change folder to %s.\n", folder);
		break;
	default:
		if (chdir(folder) != 0)
			warn_exit("can't change folder to %s.\n", folder);
		break;
	}
}

/****************************************************************
 * 
 * header sub functions
 *
 */

private char *
nextline(char *p, char *lim) {
	if (*p == LF)
		return NULL;
	while (p < lim) {
		if (*p == LF)
			break;
		p++;
	}
	p++;
	if (p < lim)
		return p;
	else {
		*(lim - 1) = LF;
		return NULL;
	}
}

private char *
nextfield(char *p, char *lim, char **truncated_end, char **prev_beg) {
	int i = 0;
	char *q;
	*truncated_end = NULL;

	do {
		q = p;
		p = nextline(p, lim);
		if (p == NULL) return NULL;
		i++;
		if (Field_len != 0 && i == Field_len)
			*truncated_end = p;
	} while (*p == SP || *p == TAB);
	if (prev_beg != NULL) *prev_beg = q;
	return p;
}

private char *
eoh(char *p, char *lim) {
	while (p < lim) {
		if (*p == LF && p + 1 < lim && *(p + 1) == LF)
			return p + 1;
		p++;
	}
	return lim - 1;
}

/****************************************************************
 * 
 * print sub-functions
 *
 */

private char **Scan_ctx_beg;
private char **Scan_ctx_end;
private char **Scan_ctx_fields;
private unsigned int *Scan_ctx_slen;
private int Scan_ctx_ref_idx = -1;
private unsigned int Scan_ctx_ptr_size;
private unsigned int Scan_ctx_fld_num;

private void
init_fields(char *fields)
{
	unsigned int i;
	char *start, *p;

	if (fields != NULL)
		p = fields;
	else
		p = DEFAULT_FIELDS;
	STRDUP(start, p);

	for (i = 1, p = start; (p = strchr(p, ',')) != NULL; i++)
		*p++ = NUL;

	Scan_ctx_fld_num = i;

	Scan_ctx_ptr_size = Scan_ctx_fld_num * sizeof(char *);

	MALLOC(Scan_ctx_fields,   Scan_ctx_ptr_size);
	MALLOC(Scan_ctx_slen,     Scan_ctx_fld_num * sizeof(int));
	MALLOC(Scan_ctx_beg,      Scan_ctx_ptr_size);
	MALLOC(Scan_ctx_end,      Scan_ctx_ptr_size);

	i = 0;
	p = start;
	do {
		Scan_ctx_fields[i] = p;
		Scan_ctx_slen[i]   = strlen(p);
		if (strcmp(p, REFERENCES) == 0) Scan_ctx_ref_idx = i;
		p = strchr(p, NUL);
		p++;
		i++;
	} while (i < Scan_ctx_fld_num);
}

private void
print_field(char *buf, int lim, char *fname, char *fld)
{
	unsigned int i;
	char *p, *limp, *end, *prev;

	memset(Scan_ctx_beg, 0, Scan_ctx_ptr_size);
	memset(Scan_ctx_end, 0, Scan_ctx_ptr_size);

	p = buf;
	limp = p + lim;

	for (;;) {
		i = 0;
		while (i < Scan_ctx_fld_num) {
			if (strncasecmp(Scan_ctx_fields[i], p,
					Scan_ctx_slen[i]) == 0) {
				Scan_ctx_beg[i] = p;
				p = nextfield(p, limp, &end, &prev);
				if (i == Scan_ctx_ref_idx) {
					if (p == NULL) {
						Scan_ctx_end[i] = limp;
						goto final;
					} else if (Scan_ctx_beg[i] == prev) {
						Scan_ctx_beg[i]
							= Scan_ctx_beg[i]
							+ Scan_ctx_slen[i];
						Scan_ctx_end[i] = p;
					} else {
						Scan_ctx_beg[i] = prev;
						Scan_ctx_end[i] = p;
					}
				} else {
					if (end != NULL) {
						Scan_ctx_end[i] = end;
						if (p == NULL) goto final;
					} else if (p == NULL) {
						Scan_ctx_end[i] = limp;
						goto final;
					} else
						Scan_ctx_end[i] = p;
				}
				break;
			}
			i++;
		}
		if (i == Scan_ctx_fld_num) {
			p = nextfield(p, limp, &end, NULL);
			if (p == NULL)
				break;
		}
		if (*p == LF) /* header ends */
			break;
	}

 final:
	if (fld != NULL)
		printf("Folder: %s\n", fld);
	printf("Filename: %s\n", fname);
	for (i = 0; i < Scan_ctx_fld_num; i++)
		if (Scan_ctx_beg[i] != NULL) {
			if (i == Scan_ctx_ref_idx) printf(REFERENCES);
			p = Scan_ctx_beg[i];
			while (p < Scan_ctx_end[i])
				putchar(*p++);
		}
	printf("\n");
}

/****************************************************************
 * 
 * String search
 *
 */

private char *Search_ctx_buf;
private int Search_ctx_lim;

private void
init_search(char *buf, int lim) {
	Search_ctx_buf = buf;
	Search_ctx_lim = lim;
}

private char *
mystrcasechr(char *s, int c)
{
	int lc = tolower(c);
	int uc = toupper(c);
	
	do {
		if (*s == lc || *s == uc)
			return s;
	} while (*s++);
	return NULL;
}

private char *
mystrcasestr(char *s1, char *s2)
{
	char *p;
	unsigned int len = strlen(s2);

	for (p = s1; (p = mystrcasechr(p, *s2)) != NULL; p++)
		if (strncasecmp(p, s2, len) == 0)
			return p;
	return NULL;
}

public int
search_string(char *key, char *value, int case_sensitive) {
	unsigned int len = strlen(key);
	char *beg = NULL, *end = NULL;
	char *p = Search_ctx_buf, *limp = p + Search_ctx_lim, tmp;

	if (strcmp(HEAD, key) == 0) {
		end = eoh(p, limp);
		tmp = *end;
		*end = NUL;
		if (case_sensitive == YES) {
			if (strstr(p, value) != NULL) {
				*end = tmp;
				return TRUE;
			}
		} else {
			if (mystrcasestr(p, value) != NULL) {
				*end = tmp;
				return TRUE;
			}
		}
		*end = tmp;
		return FALSE;
	}
		
	for (;;) {
		if (strncasecmp(key, p, len) == 0) {
			beg = p + len;
			p = nextfield(p, limp, &end, NULL);
			if (end != NULL)
				end--;		/* end == LF */
			else if (p == NULL)
				end = limp - 1;	/* end == LF, see nextline */
			else
				end = p - 1;	/* end == LF */
			tmp = *end;
			*end = NUL;
			if (case_sensitive == YES) {
				if (strstr(beg, value) != NULL) {
					*end = tmp;
					return TRUE;
				}
			} else {
				if (mystrcasestr(beg, value) != NULL) {
					*end = tmp;
					return TRUE;
				}
			}
			*end = tmp;
		} else
			p = nextfield(p, limp, &end, NULL);
		if (p == NULL)
			break;
	}
	return FALSE;
}

/****************************************************************
 * 
 * print functions
 *
 */

private int
print_for_scan(FILE *fp, char *fname, char *fld) {
	int lim = fread(Buf_header, sizeof(char), sizeof(Buf_header), fp);
	print_field(Buf_header, lim, fname, fld);
	return TRUE;
}

private int
print_for_sort(FILE *fp, char *fname, char *fld) {
	int lim = fread(Buf_header, sizeof(char), sizeof(Buf_header), fp);
	unsigned int len = strlen(Key_field);
	char *beg = NULL, *end = NULL;
	char *p = Buf_header, *limp = p + lim;

	for (;;) {
		if (strncasecmp(Key_field, p, len) == 0) {
			beg = p + len;
			p = nextfield(p, limp, &end, NULL);
			if (end != NULL)
				end--;
			else if (p == NULL) {
				end = limp;
			} else
				end = p - 1;
			break;
		}
		p = nextfield(p, limp, &end, NULL);
		if (p == NULL)
			break;
	}

	if (beg == NULL)
		printf("%s: \n", fname);
	else {
		printf("%s:", fname);
		p = beg;
		while (p < end)
			putchar(*p++);
		printf("\n");
	}

	return FALSE;
}

private int
print_for_pick(FILE *fp, char *fname, char *fld) {
	int lim = fread(Buf_header, sizeof(char), sizeof(Buf_header), fp);

	init_search(Buf_header, lim);
	if (pattern_match() == TRUE) {
		if (All_fields > 0) {
			print_field(Buf_header, lim, fname, fld);
			return TRUE;
		} else {
			printf("%s\n", fname);
			return FALSE;
		}
	}
	return FALSE;
}

/****************************************************************
 * 
 * scan functions
 *
 */

private unsigned int Scan_ctx_folders;
private unsigned int Scan_ctx_messages;
private unsigned int Scan_ctx_buf_size;
private unsigned int *Scan_ctx_buf;

private int
scan_numfile(char *s, unsigned int first, unsigned int last) {
	unsigned int num = 0;

	if (*s == NUL)
		return 0;

	while (*s != NUL) {
		if (!isdigit((unsigned char)*s))
			return 0;
		num = num * 10 + *s - '0';
		s++;
	}

	if (num >= first && (last == 0 || num <= last))
		return 1;
	else
		return 0;
}

private int
scan_intcompare(unsigned int *i, unsigned int *j) {
	if (*i > *j)
		return 1;
	if (*i < *j)
		return -1;
	return 0;
}

private int
scan_folder(struct fld_rng *fr) {
	unsigned int num = 0;
	unsigned int first = fr->first, last = fr->last;
	DIR *dirp;
	struct dirent *dp;
	char *fname;
	int i;

	ch_folder(fr->folder);

	if ((dirp = opendir(".")) == NULL)
		warn_exit("can't open folder: %s.\n", fr->folder);

	while ((dp = readdir(dirp)) != NULL) {
		fname = dp->d_name;

		if (scan_numfile(fname, first, last)) {
			if (num == Scan_ctx_buf_size) {
				Scan_ctx_buf_size += NUMBER_OF_FILE;
				Scan_ctx_buf = (unsigned int *)realloc(Scan_ctx_buf, Scan_ctx_buf_size * sizeof(unsigned int));
			}
			*(Scan_ctx_buf + num) = atoi(fname);
			num++;
		}
	}
	closedir(dirp);

	qsort(Scan_ctx_buf, num, sizeof(int),
	      (int (*)(const void *, const void *)) scan_intcompare);

	if (fr->num > 0 && fr->num < num) {
		for (i = 0; i < fr->num; i++)
			Scan_ctx_buf[i] = Scan_ctx_buf[num - fr->num + i] ;
		num = fr->num;
	}

	if (PrintNumOfMsg == YES && num > 0 && Scan_ctx_folders == 1) {
		printf("NumOfMsg: %d\n", num);
		fflush(stdout);
	}
	return num;
}

private void
scan_init(unsigned int folders)
{
	Scan_ctx_folders = folders;
	Scan_ctx_messages = 0;
	Scan_ctx_buf_size = NUMBER_OF_FILE;
	MALLOC(Scan_ctx_buf, Scan_ctx_buf_size * sizeof(int));
}

private FILE *
scan_getfile(char **filename, char **foldername)
{
	static unsigned int i = 0, j = 0;
	FILE *fp;

 again:
	if (i == Scan_ctx_messages) {
		if (j < Scan_ctx_folders) {
			Scan_ctx_messages = scan_folder(&Folders[j++]);
			i = 0;
		} else
			return NULL;
	}

	*foldername = Folders[j - 1].folder;
					    
	while (i < Scan_ctx_messages) {
		sprintf(Buf_filenm, "%d", *(Scan_ctx_buf + i));
		fp = fopen(Buf_filenm, "r");
		if (fp == NULL) {
			/* for invalid cache messages */
			sprintf(Buf_filenm, "0%d", *(Scan_ctx_buf + i));
			fp = fopen(Buf_filenm, "r");
			if (fp == NULL) {
			  i++;
			  continue;
			}
		}
		*filename = Buf_filenm;
		i++;
		return fp;
	}
	goto again;
}

/****************************************************************
 * 
 * exec functions
 *
 */

private void
exec_init(unsigned int num) /* num == 0 */
{
#ifdef HAVE_FORK
	int pipes[2];
	int childpid;
#endif /* HAVE_FORK */

	if (strlen(Folders[0].folder) >= sizeof(Current_folder))
		warn_exit("folder name is too long.\n");
	strcpy(Current_folder, Folders[0].folder);
	ch_folder(Current_folder);

#ifdef HAVE_FORK
	if (pipe(pipes) != 0) warn_exit("can't open pipe.\n");

	childpid = FORK();

	if (childpid < 0) warn_exit("can't fork.\n");

	if (childpid == 0) { /* I'm the child. */
		int i = 0;
		int lim = MAX_ARGC - 2;
		char *p;
	
		close(WRITE);
		dup(pipes[WRITE]);
		close(pipes[READ]);

		Argvec[i++] = Exec_cmd;
		if (Exec_arg != NULL) {
			p = Exec_arg;
			Argvec[i++] = p;
			while (i < lim) {
				if ((p = strchr(p, SP)) == NULL) break;
				*p++ = NUL;
				Argvec[i++] = p;
			}
		}
		Argvec[i++] = Current_folder;
		Argvec[i++] = NULL;
		
		execvp(Exec_cmd, Argvec);
		warn_exit("'%s' command not found.\n", Exec_cmd);
	}

	/* I'm the parent. */
	close(READ);
	dup(pipes[READ]);
	close(pipes[WRITE]);
#endif /* HAVE_FORK */
}

private FILE *
exec_getfile(char **filename, char **foldername)
{
	FILE *fp;
	char *p, *q;
	int c;

	*foldername = Current_folder;
	while ((p = fgets(Buf_filenm, sizeof(Buf_filenm), stdin)) != NULL) {
		if ((q = strchr(Buf_filenm, LF)) != NULL)
			*q = NUL;
		else {
			Buf_filenm[MAX_FILE_NAME_LEN - 1] = NUL;
			while ((c = getchar()) != LF && c != EOF) ;
		}
		if (STRCMP(p, CHDIR) == 0) {
			p = p + strlen(CHDIR);
			while (*p == SP || *p == TAB) p++;
			if (strlen(p) >= sizeof(Current_folder))
				warn_exit("folder name is too long.\n");
			strcpy(Current_folder, p);
			ch_folder(Current_folder);
			*foldername = Current_folder;
			continue;
		}
		while (*p == SP || *p == TAB) p++;
		if (isdigit((unsigned char)*p) == 0) continue;
		*filename = p;
		while (isdigit((unsigned char)*p)) p++;
		*p = NUL;
		fp = fopen(*filename, "r");
		if (fp != NULL) return fp;
	}
	return NULL;
}

/****************************************************************
 * 
 * stdin functions
 *
 */

private void
stdin_init(unsigned int num) /* num == 0 */
{
	if (strcmp(Input_file, STDINFILE) != 0)
		if (freopen(Input_file, "r", stdin) == NULL)
			warn_exit("can't open %s.\n", Input_file);

	if (strlen(Folders[0].folder) >= sizeof(Current_folder))
		warn_exit("folder name is too long.\n");
	strcpy(Current_folder, Folders[0].folder);
	ch_folder(Current_folder);
}

/****************************************************************
 * 
 * parser for folder and range
 *
 */

private void
set_fld_rng(struct fld_rng *fr, char *arg1, char *arg2)
{
	char *folder = arg1;
	char *range = NULL, *last = NULL;

	if (arg1 != NULL) range = strchr(arg1, SP);
	if (range == NULL)
		range = arg2;
	else
		*range++ = NUL;
	if (range == NULL)
		fr->first = fr->last = fr->num = 0;
	else if (strncmp(range, "last:", 5) == 0) {
		range += 5;
		fr->first = fr->last = 0;
		fr->num = atoi(range);
	} else if ((last = strchr(range, '-')) == NULL) {
		fr->first = fr->last = atoi(range);
		fr->num = 0;
	} else {
		*last++ = NUL;
		fr->first = atoi(range);
		fr->last = atoi(last);
		fr->num = 0;
	}

	if (folder == NULL) folder = INBOX;
	STRDUP(fr->folder, folder);
}

/****************************************************************
 * 
 * main
 *
 */

int
main(int argc, char **argv)
{
	int optc, rest;
	int eflag = 0, nflag = 1, sflag = 0;
	int wait;
	useconds_t wait_for = 0;
	unsigned int lim = 0;
	struct stat st;
	int  (*func_print)(FILE *, char *, char *);
	void (*func_init)(unsigned int);
	FILE *(*func_getfile)(char **, char **);
	FILE *fp;
	char *filename, *foldername, *fields = NULL, *pattern = NULL;
	char *progname = getprognm(argv[0]);
	char *fld_rng;

	warn_prog = progname;
	STRDUP(Mail_home, MAILHOME);

	while ((optc = Getopt(argc, argv, "ab:c:d:e:f:hi:l:m:np:s:vw:")) != EOF) {
		switch (optc) {
		case 'a':
			All_fields++;
			break;
		case 'b':
			STRDUP(Mail_home, Optarg);
			break;
		case 'c':
			/* just ignore for backward compatibility */
			break;
		case 'd':
			STRDUP(Key_field, Optarg);
			break;
		case 'e':
			STRDUP(Exec_cmd, Optarg);
			eflag++;
			break;
		case 'f':
			STRDUP(fields, Optarg);
			PrintNumOfMsg = YES;
			break;
		case 'h':
			help(progname);
			exit(EXIT_SUCCESS);
			break;
		case 'i':
			STRDUP(Input_file, Optarg);
			break;
		case 'l':
			Field_len = atoi(Optarg);
			break;
		case 'm':
			STRDUP(Exec_arg, Optarg);
			break;
		case 'n':
			nflag = 0;
			break;
		case 'p':
			STRDUP(pattern, Optarg);
			break;
		case 's':
			if (lim >= MAX_FOLDER)
				warn_exit("too many folders.\n");
			STRDUP(fld_rng, Optarg);
			set_fld_rng(&Folders[lim++], fld_rng, NULL);
			sflag++;
			break;
		case 'v':
			version(progname);
			exit(EXIT_SUCCESS);
			break;
		case 'w':
			wait_for = atoi(Optarg);
			break;
		default:
			usage(progname);
			exit(EXIT_FAILURE);
		}
	}

	rest = argc - Optind;

	if (sflag == 0) 
		switch (rest) { /* lim == 0 */
		case 0:
			set_fld_rng(&Folders[lim++], NULL, NULL);
			break;
		case 1:
			set_fld_rng(&Folders[lim++], argv[Optind], NULL);
			break;
		case 2:
			set_fld_rng(&Folders[lim++], argv[Optind], argv[Optind + 1]);
			break;
		default:
			usage(progname);
			exit(EXIT_FAILURE);
		}
	else if (rest != 0)
		warn_exit("the 's' option can't co-exist with arguments.\n");

	if (Input_file != NULL) {
		func_init = stdin_init;
		func_getfile = exec_getfile;
	} else if (eflag > 0) {
		func_init = exec_init;
		func_getfile = exec_getfile;
	} else {
		func_init = scan_init;
		func_getfile = scan_getfile;
	}

	init_fields(fields); /* lengthy sometime */
	
	if (pattern != NULL) {
		pattern_init(pattern);
		func_print = print_for_pick;
		PrintNumOfMsg = NO;
	} else if (Key_field != NULL)
		func_print = print_for_sort;
	else
		func_print = print_for_scan;
		
	(*func_init)(lim);
	while ((fp = (*func_getfile)(&filename, &foldername)) != NULL) {
		if (nflag != 0) {
			fstat(fileno(fp), &st);
			if (!(st.st_mode & S_IFREG)) {
				fclose(fp);
				continue;
			}
		}
		wait = (*func_print)(fp, filename, foldername);
		if (wait == TRUE && wait_for != 0) {
			usleep(wait_for);
			fflush(stdout);
		}
		fclose(fp);
	}

	exit(EXIT_SUCCESS);
}

/* 
 * Copyright (C) 2000-2003 Mew developing team.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the team nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * mewls.c ends here
 */
