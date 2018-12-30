source ~/shell/produce_lualatex_func.sh
LECTURESSOURCEPATH=~/Documents/textbooks/

cd $LECTURESSOURCEPATH
echo "Producing Banking lectures..."
for i in 1 2 3 
do
    cd banking/tex/$i
    ./produce.sh
    cd $LECTURESSOURCEPATH
done

cd banking/tex/Control
./produce.sh
cd $LECTURESSOURCEPATH

echo "Producing Exercises..."
cd exercises 
./produce.sh 
cd $LECTURESSOURCEPATH

echo "Producing DKB lectures..."
for i in 1 2 
do
    cd dkb/tex/$i && \
    ./produce.sh && \
    cd $LECTURESSOURCEPATH
done

cd dkb/tex/Control 
./produce.sh 
cd $LECTURESSOURCEPATH

echo "Producing Financial Decisions (FD) lectures..."
cd fd/tex/1
./produce.sh
cd $LECTURESSOURCEPATH

cd fd/tex/Control
./produce.sh && 
cd $LECTURESSOURCEPATH

echo "Producing Financial Risks (FR) lectures..."
cd fr/tex/1
./produce.sh
cd $LECTURESSOURCEPATH
# controls are in separate files (old form) without tag \professor
#cd FR/tex/Control && ./produce.sh && cd $LECTURESSOURCEPATH

echo "Producing International Finance (IF) lectures..."
for i in 1 2 
do
    cd if/tex/$i
    ./produce.sh
    cd $LECTURESSOURCEPATH
done

cd if/tex/Control 
./produce.sh 
cd $LECTURESSOURCEPATH

echo "Producing MVKO lectures..."
# no latex lectures
#cd MVKO/tex/1 && ./produce.sh && cd $LECTURESSOURCEPATH
cd mvko/tex/Control && ./produce.sh && cd $LECTURESSOURCEPATH

echo "Producing Venture Capital (VC) lectures..."
for i in {1..4}
do
    cd vc/tex/$i
    ./produce.sh
    cd $LECTURESSOURCEPATH
done

cd vc/tex/Control 
./produce.sh 
cd $LECTURESSOURCEPATH

