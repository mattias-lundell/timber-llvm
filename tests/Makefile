TIMBERC=timberc

FILES = Synstest.t Solitaire.Card.t Solitaire.CardPile.t AbstractStack.t AbstractStackClient.t Observer.t ImportChain1.t ImportChain2.t ImportChain3.t Conflict.t ConflictClient.t SizedPoint.t SizedPoint2.t Tail.t Twice.t NoInteraction.t Array0.t Histo.t HistoClient.t Matrix.t  ManySimpleConstraints.t KindleArities.t Pointed.t Cyclic.t StructPat.t Binary.t Binary2.t  A.t B1.t B2.t C1.t C2.t C3.t C4.t ActionParameter.t BeforeConstraint.t BitOpsDefaulting.t DataExtension.t Defaulting.t EmptyStruct.t FloatFoldr.t Forall.t Forall2.t GlobalDo.t Guards.t HigherOrderCircularList.t LocalMono.t LocalSig.t MonoValueRec.t MutualInst.t MutualInst2.t PatBinds.t PublicPrivateSubtyping.t RecursiveMethods.t ShouldDelay.t ShouldNotDelay.t SigShuffling.t SimpleArray.t StateVarSig.t StringEscapes.t StructStuffing.t SubtypedStructsAndData.t

tests:
	@for ex in $(FILES); do \
		$(TIMBERC) -c  $$ex; \
	done

local:
	@for ex in $(FILES); do \
		../timberc -c  $$ex; \
	done
