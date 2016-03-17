echo "git add ."
git add .
echo "input commit message: "
read msg
echo "git commit -m $msg"
git commit -m $msg
"git push origin master"
git push origin master