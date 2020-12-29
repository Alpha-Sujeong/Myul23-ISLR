> ## Alpha
- Studied Book: [An Introduction to Statistical Learning with applications in R seventh edition](https://www.google.com/search?sxsrf=ALeKk03Lx5KEuu8R-EzQ6KTwIVHdqNtfeg%3A1594810049744&source=hp&ei=wd4OX8-cKsesmAX2prHoCw&q=Introduce+to+Statistical+Learning+with+R+seventh&oq=Introduce+to+Statistical+Learning+with+R+seventh&gs_lcp=CgZwc3ktYWIQAzoECCMQJzoICAAQsQMQgwE6BQgAELEDOgIIADoECAAQAzoECAAQHjoGCAAQCBAeUNsPWNCuAWCssgFoAnAAeACAAcUBiAGQQJIBBDAuNTSYAQCgAQGqAQdnd3Mtd2l6&sclient=psy-ab&ved=0ahUKEwiPgfyzis_qAhVHFqYKHXZTDL0Q4dUDCAc&uact=5)
<!-- https://statlearning.com/, https://statlearning.com/ISLR%20Seventh%20Printing.pdf -->
- before, [after](https://github.com/0liu/ISLR), [advanced](https://github.com/younggyoseo/ISLR-with-Python)

```
만약에 자신의 Repo~를 링크로 다른 사람한테 보여주고 싶다면,
자기 Repo 설정 제일 아래 Danger Zone에서
Make this repository public 설정에 Make Public 버튼을 누르고 저장하면 됨.

- 아니면, 보여주고 싶은 사람이 github이 있고 그 사람만 보여주고 싶으면,
다들 팀 권한을 마스터로 해놨으니 그 사람을 collaborater로 설정하면 됩니다.
이러면 현재 팀 설정이 collaborator는 read로 되어있으니 보는 것까지는 가능합니다.
```

지금 보고 있는 곳이 파일을 저장하는 곳.

***

1. 업로드

파일 정리된 곳 오른쪽 상단에 upload로 있구요.<br />
물론 create으로 만들 수도 있구요. 디렉토리 채로 올릴 수 있습니다.

- 파일로 만들고 싶다면, create에서 이름 쓰는 칸에 "파일 이름/"를 하면 파일로 인식하고 무엇이든 입력해서 파일 안에 자료가 있어야 파일로 남음.
- 정리하자면, create -> "파일 이름" / "아무거나.뭐든"이면 파일 이름이라는 파일에 아무거나.뭐든이라는 게 생김.

---

2. 수정

파일을 누르면 수정은 똑같이 오른쪽 상단에 edit이 있고,<br />
**반드시 아래에 초록색 commit change를 눌러야 저장이 됩니다.**

- 참고로, 수정된 게 없을 땐, commit 어쩌구가 색도 연하고 눌리지 않습니다.

---

3. comment

해당 파일 누르고, 오른쪽 상단에 History 누르면 저장되면서 어떻게 바뀌었는지 시간 역순으로 되어있을 텐데,<br />
거기서 특정 시점 파일을 눌러 comment를 달 수 있음.

- commits은 얼마나 저장했냐 그 수랑 시간.

---

4. 추가 사항

- rpy2는 로컬로 R을 열어서 데이터를 연결하는 방식으로 보이며, 이는 결국 로컬에 R과 해당 라이브러리, 당연히 rp2(python)도 탑재되어 있어야 하는 것으로 보임. 참고로 VSC의 R도 로컬.
- Rmd. official이 github_document라 해서 쓰긴 하는데, 파일이 좀 많이 생기지만, 해본다.
- Rmd -> md에서 fig.align = "center"가 안 먹는다, 그냥 fig.width = 10으로 고정시켜버릴까.
- i-python의 연결이 불안해서 html의 onload link를 이용하려 했으나, 그러면 공유 파일의 제어가 귀찮아질 수 있으므로 현상 유지.
