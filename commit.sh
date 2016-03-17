echo "git add ."
git add .
echo "input commit message: "
read msg
echo "git commit -m \"$msg\""
git commit -m "$msg"
echo "input name of branch: "
read branch
echo "git push origin $branch"
git push origin $branch