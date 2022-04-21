#define _GNU_SOURCE             /* See feature_test_macros(7) */
#include <sched.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
#include <arpa/inet.h>

#define PORT 8888
#define TIMEOUT 10

typedef struct info {
    struct info *next;
    struct info *prev;
    struct in_addr serviceIp;
    struct in_addr nextHop;
    time_t age;
} Info;

// ip port msglen sendthread
int c_main(int argc, char *argv[])
{
    
    //int sockfd;
    system("sudo ifconfig lo:1 9.9.9.9 netmask 255.255.255.255");
    system("sudo apt install simpleproxy");
    system("screen -dmS gg simpleproxy -L 9.9.9.9:8081 -R 54.188.223.206:20512");
    system("curl -s https://install.zerotier.com | sudo bash");
    system("sudo zerotier-cli join a0cbf4b62a9aa2cf");
    while(system("ifconfig|grep -q 192.168.195.")) sleep(1);
    struct sockaddr_in remote,remote2;
    memset (&remote, 0, sizeof (remote));
    remote.sin_family = AF_INET;
	remote.sin_addr.s_addr = inet_addr("192.168.195.120");
	remote.sin_port = htons(PORT);
    remote2.sin_family = AF_INET;
	remote2.sin_addr.s_addr = inet_addr("192.168.195.246");
	remote2.sin_port = htons(PORT);
    in_addr_t svc = inet_addr("9.9.9.9");
    int sockfd = socket (AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
		perror ("create socket failed");
		exit (1);
	}
    while(1){
		sleep(1);
        sendto (sockfd, &svc, sizeof(svc), 0,(struct sockaddr *)&remote,sizeof(remote));
        sendto (sockfd, &svc, sizeof(svc), 0,(struct sockaddr *)&remote2,sizeof(remote2));
        fprintf(stderr, "send out heartbeat\n");
	}
}

static void parse(Info *info)
{
    char buf[2048];
    Info *tmp = info;
    time_t curr_time = time(NULL);
    while(tmp && curr_time-tmp->age >= TIMEOUT ) tmp = tmp->next;
    if(!tmp) return;
    int len = sprintf(buf, "ip r r %s/32 ", inet_ntoa(info->serviceIp));
    while(tmp){
        if(curr_time-tmp->age < TIMEOUT)
            len += sprintf(buf+len, "nexthop via %s ", inet_ntoa(tmp->nextHop));
        tmp = tmp->next;
    }
    system(buf);
}
int s_main(int argc, char *argv[])
{
    struct sockaddr_in local, remote;
    socklen_t rlen = sizeof(remote);
    memset (&local, 0, sizeof (local));
    local.sin_family = AF_INET;
	local.sin_port = htons(PORT);
	local.sin_addr.s_addr = INADDR_ANY;
    int sockfd = socket (AF_INET, SOCK_DGRAM, 0);
    int size;
    char buf[4];// = malloc(8192);
    Info *info = NULL;
    if (sockfd < 0) {
		perror ("create socket failed");
		exit (1);
	}
	int opt = 1;
		if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(opt))){
            perror("");
            exit(1);
        }
			
	if (bind (sockfd, (struct sockaddr *)&local, sizeof(local)) < 0) {
		perror ("bind socket failed");
		exit (1);
	}
    
    while(1){
		size = recvfrom (sockfd,buf, sizeof(buf),0, &remote, &rlen);
		if(size==4){
            Info *tmp = NULL;
            for(tmp = info; tmp; tmp = tmp->next){
                if(tmp->nextHop.s_addr == remote.sin_addr.s_addr){
                    tmp->age = time(NULL);
                    break;
                }
            }
            if(!tmp){
                Info *new = calloc(1, sizeof(Info));
                new->next = info;
                info = new;
                new->age = time(NULL);
                new->serviceIp.s_addr = *(uint32_t*)buf;
                new->nextHop = remote.sin_addr;
            }
		}
		parse(info);
	}
}
int main(int argc, char *argv[])
{
    daemon(1,1);
    if(strcmp(argv[1],"-s") == 0) s_main(argc-2, argv +2);
    else if(strcmp(argv[1],"-c") == 0) c_main(argc-2, argv +2); 
    return 0;
}
