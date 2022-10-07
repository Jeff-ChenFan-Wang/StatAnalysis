# How to use git (very basics)

## Typical workflow
 1. Get the work everyone has done first <br>``` git pull```
 2. Do you own work 
 3. Add all your changes to a snapshot <br>```git add -A```
 4. Make a snapshot of what your folder looks like right now with all your changes<br>```git commit -m "describe what you did here"```
 5. Upload the changes online so others can see<br>```git push```


## Clone Repository 
You only need to do this ONCE when you first start working with our github project <br>
```git clone https://github.com/Jeff-ChenFan-Wang/StatAnalysis.git```

## Create new branch 
You want to do this if you are working on something separately 
 1. create your branch ```git checkout -b branchName```
 2. ```git add -A```
 3. ```git commit -m "describe what you did"```
 4. ```git push --set-upstream origin branchName```
